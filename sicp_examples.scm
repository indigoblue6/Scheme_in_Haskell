;; SICP Examples - クリーン版（テスト用）

;; 1. フィボナッチ数列（再帰版）
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(begin
  (display "1. Fibonacci (recursive): fib(10) = ")
  (display (fib 10))
  (newline))

;; 2. フィボナッチ数列（反復版、末尾再帰）
(define (fib-iter n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

(begin
  (display "2. Fibonacci (iterative): fib(10) = ")
  (display (fib-iter 10))
  (newline))

;; 3. 最大公約数
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(begin
  (display "3. GCD: gcd(48, 18) = ")
  (display (gcd 48 18))
  (newline))

;; 4. 素数判定
(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (* test-divisor test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (and (> n 1) (= n (smallest-divisor n))))

(begin
  (display "4. Prime test: (prime? 17) = ")
  (display (prime? 17))
  (display ", (prime? 18) = ")
  (display (prime? 18))
  (newline))

;; 5. フィルタ
(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(begin
  (display "5. Filter: (filter even? '(1 2 3 4 5 6)) = ")
  (display (filter even? '(1 2 3 4 5 6)))
  (newline))

;; 6. アキュムレート
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(begin
  (display "6. Accumulate: sum of (1 2 3 4 5) = ")
  (display (accumulate + 0 '(1 2 3 4 5)))
  (newline))

;; 7. フリンジ
(define (fringe tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(begin
  (display "7. Fringe: (fringe '((1 2) (3 (4 5)))) = ")
  (display (fringe '((1 2) (3 (4 5)))))
  (newline))

;; 8. Yコンビネータ
(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (y) ((x x) y))))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

(define fact-gen
  (lambda (f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))))

(begin
  (display "8. Y-combinator: 5! = ")
  (display ((Y fact-gen) 5))
  (newline))

;; 9. メモ化フィボナッチ
(define (make-memo-fib)
  (let ((memo '((0 . 0) (1 . 1))))
    (lambda (n)
      (let ((cached (assoc n memo)))
        (if cached
            (cdr cached)
            (let ((result
                   (cond ((= n 0) 0)
                         ((= n 1) 1)
                         (else (+ ((make-memo-fib) (- n 1))
                                  ((make-memo-fib) (- n 2)))))))
              (set! memo (cons (cons n result) memo))
              result))))))

(begin
  (display "9. Memoized Fibonacci: fib(20) = ")
  (display ((make-memo-fib) 20))
  (newline))

(display "\n=== All SICP examples completed! ===\n")
