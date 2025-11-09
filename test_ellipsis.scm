; Ellipsis (省略記号) のテスト

; 1. 単純なドット3つ
(define (test-macro)
  (syntax-rules ()
    ((test-macro x ...)
     (list x ...))))

; 2. 閉じ括弧の前にスペース
(+ 1 2 )

; 3. 複雑なネスト
(let ((x 1))
  (let ((y 2))
    (+ x y )))

; 4. 元の問題のケース
(define (my-test-function x)
  (+ x ))

(my-test-function 10)
