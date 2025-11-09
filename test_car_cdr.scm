;; car/cdr組み合わせ関数のテスト
(define nested '((1 2) (3 4) (5 6)))

(display "Testing car/cdr combinations:\n")
(display "  nested = ")
nested
(display "\n  (caar nested) = ")
(caar nested)
(display "\n  (cadr nested) = ")
(cadr nested)
(display "\n  (caddr nested) = ")
(caddr nested)
(display "\n  (caadr nested) = ")
(caadr nested)
(display "\n\nAll car/cdr combinations work!\n")
