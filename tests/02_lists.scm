;; Test Suite 02: List Operations

;; Basic list functions
(define (test-basic-lists)
  (and (= (car '(1 2 3)) 1)
       (equal? (cdr '(1 2 3)) '(2 3))
       (equal? (cons 1 '(2 3)) '(1 2 3))
       (= (length '(1 2 3 4)) 4)
       (equal? (reverse '(1 2 3)) '(3 2 1))
       (equal? (append '(1 2) '(3 4)) '(1 2 3 4))))

;; car/cdr combinations
(define (test-car-cdr-combos)
  (let ((nested '((1 2) (3 4) (5 6))))
    (and (= (caar nested) 1)
         (equal? (cadr nested) '(3 4))
         (equal? (caddr nested) '(5 6))
         (= (caadr nested) 3))))

;; List predicates
(define (test-list-predicates)
  (and (null? '())
       (not (null? '(1 2 3)))
       (list? '(1 2 3))
       (pair? '(1 . 2))
       (not (list? 'symbol))))

;; List accessors
(define (test-list-accessors)
  (and (equal? (list-ref '(a b c d) 2) 'c)
       (equal? (list-tail '(a b c d) 2) '(c d))))

;; Run all tests
(display "\nTesting List Operations:\n")
(display "  Basic lists: ")
(if (test-basic-lists) (display "PASS\n") (display "FAIL\n"))
(display "  car/cdr combos: ")
(if (test-car-cdr-combos) (display "PASS\n") (display "FAIL\n"))
(display "  Predicates: ")
(if (test-list-predicates) (display "PASS\n") (display "FAIL\n"))
(display "  Accessors: ")
(if (test-list-accessors) (display "PASS\n") (display "FAIL\n"))
