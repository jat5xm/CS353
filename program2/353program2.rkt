#lang racket

;parses a CSV file and return a list of lists representing the data
(define (parse-csv filename)
  (call-with-input-file filename
    (lambda (input)
      (let loop ((lines (port->lines input)) (data '()))
        (if (null? lines)
            (reverse data)
            (loop (cdr lines)
                  (cons (string-split (car lines) ",") data)))))))

; parses the CSV file and stores the data in a variable
(define data-set (parse-csv "Video Games Sales.csv"))

; search for games by their name
(define (search-by-name name)
  (filter (lambda (row) (string-ci=? (list-ref row 2) name)) data-set))

; search for games by their release date
(define (search-by-date start-year end-year)
  (if (< end-year start-year)
      (search-by-date end-year start-year)
      (filter (lambda (row) (and (>= (list-ref row 4) start-year) (<= (list-ref row 4) end-year))) data-set)))

; search for games by their publisher
(define (search-by-publisher publisher)
  (filter (lambda (row) (string-ci-contains? (list-ref row 6) publisher)) data-set))

; search for games by their region
(define (search-by-region region)
  (case (string-downcase region)
    ((north america) (filter (lambda (row) (not (= (list-ref row 7) 0))) data-set))
    ((europe) (filter (lambda (row) (not (= (list-ref row 8) 0))) data-set))
    ((japan) (filter (lambda (row) (not (= (list-ref row 9) 0))) data-set))
    ((rest of world) (filter (lambda (row) (not (= (list-ref row 10) 0))) data-set))
    ((global) data-set)))

; search for games by their genre
(define (search-by-genre genre)
  (filter (lambda (row) (string-ci=? (list-ref row 5) genre)) data-set))

; checks if a string contains a substring (case insensitive)
(define (string-ci-contains? str substr)
  (string-ci=? substr (substring str 0 (min (string-length str) (string-length substr)))))

; displays results ordered by rank or review score
(define (display-results results order-by) ; sorts the results based on the specified order
  (define (sort-by-rank)
    (sort results (lambda (a b) (< (list-ref a 1) (list-ref b 1)))))
  
  (define (sort-by-review)
    (sort results (lambda (a b) (> (list-ref a 12) (list-ref b 12)))))
  
  (case order-by ; displays the sorted results
    ((rank) (display (sort-by-rank)))
    ((review) (display (sort-by-review)))))

; gets search criteria from the user
(define (get-criteria)
  (let ((criteria (list)))  ; collect up to 3 search criteria from the user
    (for ([i (in-range 3)]) 
      (printf "Enter search criteria (~a of 3): " (+ i 1))
      (let* ((choice (string-downcase (read-line)))) ; processes user input and adds corresponding search results to criteria list
        (cond
          ((equal? choice "name")
           (set! criteria (cons (search-by-name (string-downcase (read-line))) criteria)))
          ((equal? choice "date")
           (set! criteria (cons (search-by-date (read) (read)) criteria)))
          ((equal? choice "publisher")
           (set! criteria (cons (search-by-publisher (string-downcase (read-line))) criteria)))
          ((equal? choice "region")
           (set! criteria (cons (search-by-region (string-downcase (read-line))) criteria)))
          ((equal? choice "genre")
           (set! criteria (cons (search-by-genre (string-downcase (read-line))) criteria)))
          (else
           (printf "Invalid choice. Please try again.\n")))))
    criteria)) ; returns the collected criteria


; gets the user's order preference
(define (get-order-by)
  (printf "Order by (rank/review): ")
  (string-downcase (read-line)))

; checks if the user wants to continue or quit
(define (continue?)
  (printf "Do you want to continue? (yes/no): ")
  (string-ci=? (string-trim (read-line)) "yes"))

; just a regular main function to control and execute the program
(define (main)
  (let loop ()
    (let* ((criteria (get-criteria)) ; gets search criteria and order preference from the user
           (order-by (get-order-by)))
      (for ([c criteria]) ; displays the results for each criteria according to the specified order
        (display-results c order-by))
      (if (continue?) ; checks if the user wants to continue or quit the program
          (loop)
          (printf "Goodbye!\n")))))

(main)
