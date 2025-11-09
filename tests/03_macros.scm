;; Test Suite 03: Macros

;; Test simple macro
(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...)))))

(define (test-when-macro)
  (let ((x 0))
    (when #t (set! x 1))
    (= x 1)))

;; Test ellipsis pattern
(define-syntax let-values
  (syntax-rules ()
    ((let-values ((var val) ...) body ...)
     (let ((var val) ...) body ...))))

(define (test-ellipsis-macro)
  (let-values ((x 1) (y 2) (z 3))
    (= (+ x y z) 6)))

;; Run all tests
(display "\nTesting Macros:\n")
(display "  When macro: ")
(if (test-when-macro) (display "PASS\n") (display "FAIL\n"))
(display "  Ellipsis pattern: ")
(if (test-ellipsis-macro) (display "PASS\n") (display "FAIL\n"))
