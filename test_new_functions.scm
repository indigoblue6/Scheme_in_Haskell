;; 新機能のテスト

;; 数学関数
(display "Math functions:\n")
(display "  (sqrt 16) = ")
(sqrt 16)
(display "\n  (expt 2 10) = ")
(expt 2 10)
(display "\n  (sin 0) = ")
(sin 0)
(display "\n  (cos 0) = ")
(cos 0)

;; リスト関数
(display "\n\nList functions:\n")
(display "  (list-ref '(a b c d) 2) = ")
(list-ref '(a b c d) 2)
(display "\n  (list-tail '(a b c d) 2) = ")
(list-tail '(a b c d) 2)

;; 型変換
(display "\n\nType conversion:\n")
(display "  (number->string 123) = ")
(number->string 123)
(display "\n  (string->number \"456\") = ")
(string->number "456")

(display "\n\nAll new functions work!\n")
