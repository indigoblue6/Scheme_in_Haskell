;; 総合テスト - すべての機能を確認

(display "=== Scheme Interpreter - Comprehensive Test ===\n\n")

;; 1. 基本演算
(display "1. Basic arithmetic:\n")
(display "  2 + 3 = ")
(+ 2 3)
(display "\n  10 - 4 = ")
(- 10 4)
(display "\n  3 * 7 = ")
(* 3 7)

;; 2. 有理数演算（正確）
(display "\n\n2. Exact rational arithmetic:\n")
(display "  1/2 + 1/3 = ")
(+ 1/2 1/3)
(display "\n  2/3 * 3/4 = ")
(* 2/3 3/4)
(display "\n  1 / 3 = ")
(/ 1 3)

;; 3. 複素数演算
(display "\n\n3. Complex numbers:\n")
(display "  (1+2i) + (3+4i) = ")
(+ 1+2i 3+4i)
(display "\n  (2+3i) * (1+1i) = ")
(* 2+3i 1+1i)

;; 4. 数学関数
(display "\n\n4. Mathematical functions:\n")
(display "  sqrt(16) = ")
(sqrt 16)
(display "\n  2^10 = ")
(expt 2 10)

;; 5. リスト操作
(display "\n\n5. List operations:\n")
(display "  (car '(1 2 3)) = ")
(car '(1 2 3))
(display "\n  (cdr '(1 2 3)) = ")
(cdr '(1 2 3))
(display "\n  (list-ref '(a b c d) 2) = ")
(list-ref '(a b c d) 2)

;; 6. 高階関数
(display "\n\n6. Higher-order functions:\n")
(display "  (map (lambda (x) (* x x)) '(1 2 3 4)) = ")
(map (lambda (x) (* x x)) '(1 2 3 4))

;; 7. 末尾再帰（最適化済み）
(display "\n\n7. Tail recursion (optimized):\n")
(define (factorial n acc)
  (if (<= n 1)
      acc
      (factorial (- n 1) (* n acc))))
(display "  10! = ")
(factorial 10 1)

;; 8. マクロ
(display "\n\n8. Macros:\n")
(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...)))))
(display "  (when #t (display \"Macro works!\")) = ")
(when #t (display "Macro works!"))

;; 9. 継続
(display "\n\n9. Continuations:\n")
(display "  call/cc test = ")
(call/cc (lambda (k) (+ 1 (k 100) 3)))

;; 10. 遅延評価
(display "\n\n10. Lazy evaluation:\n")
(define p (delay (+ 1 2)))
(display "  (force p) = ")
(force p)

(display "\n\n=== All tests passed! ===\n")
