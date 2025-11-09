;; Test Suite 04: Advanced Features

;; Test call/cc
(define (test-call-cc)
  (= (call/cc (lambda (k) (+ 1 (k 100) 3))) 100))

;; Test delay/force
(define (test-delay-force)
  (let ((p (delay (+ 1 2))))
    (= (force p) 3)))

;; Test tail recursion
(define (test-tail-recursion)
  (define (sum-to-n n acc)
    (if (<= n 0)
        acc
        (sum-to-n (- n 1) (+ acc n))))
  (= (sum-to-n 1000 0) 500500))

;; Run all tests
(display "\nTesting Advanced Features:\n")
(display "  call/cc: ")
(if (test-call-cc) (display "PASS\n") (display "FAIL\n"))
(display "  delay/force: ")
(if (test-delay-force) (display "PASS\n") (display "FAIL\n"))
(display "  Tail recursion: ")
(if (test-tail-recursion) (display "PASS\n") (display "FAIL\n"))
