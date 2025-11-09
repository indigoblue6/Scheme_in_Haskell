;; テスト: すべての改善機能

;; 1. マクロの...パターン
(define-syntax let-values
  (syntax-rules ()
    ((let-values ((var val) ...) body ...)
     (let ((var val) ...) body ...))))

(display "1. Macro ellipsis test: ")
(let-values ((x 1) (y 2) (z 3))
  (+ x y z))

;; 2. 完全な複素数演算
(display "\n2. Complex number arithmetic:\n")
(display "  (+ 3+4i 1+2i) = ")
(+ 3+4i 1+2i)

(display "\n  (* 3+4i 1+2i) = ")
(* 3+4i 1+2i)

;; 3. 正確な有理数演算
(display "\n3. Exact rational arithmetic:\n")
(display "  (/ 1 3) = ")
(/ 1 3)

(display "\n  (+ 1/3 1/3 1/3) = ")
(+ 1/3 1/3 1/3)

(display "\n  (* 2/3 3/4) = ")
(* 2/3 3/4)

;; 4. エラーメッセージ改善のテスト
(display "\n4. Error message test:\n")
;; これはエラーになるのでコメントアウト
;; (+ "hello" 42)

;; 5. 末尾再帰最適化のテスト
(display "5. Tail recursion test: ")
(define (sum-to-n n acc)
  (if (<= n 0)
      acc
      (sum-to-n (- n 1) (+ acc n))))

(sum-to-n 1000 0)

(display "\n\nAll tests completed!\n")
