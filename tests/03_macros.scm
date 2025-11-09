;; Test Suite 03: Macros

;; Test simple macro
(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...)))))

;; Test when macro directly
(begin
  (display "Testing Macros:\n")
  (display "  When macro: "))

(define x 0)
(when #t (set! x 1))
(if (= x 1)
    (display "PASS\n")
    (display "FAIL\n"))

;; Test ellipsis pattern
(define-syntax let-values
  (syntax-rules ()
    ((let-values ((var val) ...) body ...)
     (let ((var val) ...) body ...))))

(display "  Ellipsis pattern: ")

(let-values ((a 1) (b 2) (c 3))
  (if (= (+ a b c) 6)
      (display "PASS\n")
      (display "FAIL\n")))
