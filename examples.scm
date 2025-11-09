; Scheme Examples for the Scheme Interpreter

; 1. 算術演算の例
(+ 1 2 3 4 5)        ; => 15
(* 2 3 4)            ; => 24
(- 10 3)             ; => 7
(/ 20 4)             ; => 5

; 2. 変数定義
(define x 42)
(define y 10)
(+ x y)              ; => 52

; 3. 関数定義
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 5)        ; => 120

; 4. 階乗の別実装（ラムダ式を使用）
(define fact
  (lambda (n)
    (if (= n 0)
        1
        (* n (fact (- n 1))))))

(fact 6)             ; => 720

; 5. リスト操作
(car '(1 2 3))       ; => 1
(cdr '(1 2 3))       ; => (2 3)
(cons 0 '(1 2 3))    ; => (0 1 2 3)

; 6. 条件分岐
(define (abs x)
  (if (< x 0)
      (- 0 x)
      x))

(abs -5)             ; => 5
(abs 7)              ; => 7

; 7. フィボナッチ数列
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib 10)             ; => 55

; 8. 高階関数の例
(define (twice f x)
  ((lambda (y) (f (f y))) x))

(define (inc x) (+ x 1))
(twice inc 5)        ; => 7

; 9. リスト処理
(define (length lst)
  (if (eq? lst '())
      0
      (+ 1 (length (cdr lst)))))

(length '(1 2 3 4 5)) ; => 5

; 10. 最大値を求める関数
(define (max a b)
  (if (> a b) a b))

(max 10 20)          ; => 20
(max 100 50)         ; => 100
