;; Test Suite 01: Arithmetic Operations

;; Test integers
(define (test-integers)
  (and (= (+ 1 2) 3)
       (= (- 5 3) 2)
       (= (* 3 4) 12)
       (= (quotient 10 3) 3)
       (= (remainder 10 3) 1)
       (= (mod 10 3) 1)))

;; Test rationals (exact)
(define (test-rationals)
  (and (equal? (+ 1/2 1/3) 5/6)
       (equal? (- 3/4 1/4) 1/2)
       (equal? (* 2/3 3/4) 1/2)
       (equal? (/ 1 3) 1/3)
       (equal? (+ 1/3 1/3 1/3) 1)))

;; Test complex numbers
(define (test-complex)
  (and (equal? (+ 1+2i 3+4i) 4+6i)
       (equal? (- 5+6i 2+3i) 3+3i)
       (equal? (* 2+0i 3+0i) 6+0i)))

;; Test floats
(define (test-floats)
  (and (= (+ 1.5 2.5) 4.0)
       (= (- 5.5 2.5) 3.0)
       (= (* 2.0 3.0) 6.0)))

;; Run all tests
(display "Testing Arithmetic Operations:\n")
(display "  Integers: ")
(if (test-integers) (display "PASS\n") (display "FAIL\n"))
(display "  Rationals: ")
(if (test-rationals) (display "PASS\n") (display "FAIL\n"))
(display "  Complex: ")
(if (test-complex) (display "PASS\n") (display "FAIL\n"))
(display "  Floats: ")
(if (test-floats) (display "PASS\n") (display "FAIL\n"))
