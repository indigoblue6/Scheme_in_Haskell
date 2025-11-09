;; Test equal? with rationals and complex numbers

(begin
  (display "Testing equal? function:\n")
  (display "  Rational 1/2 + 1/3 = 5/6: ")
  (display (equal? (+ 1/2 1/3) 5/6))
  (newline)
  
  (display "  Complex 1+2i + 3+4i = 4+6i: ")
  (display (equal? (+ 1+2i 3+4i) 4+6i))
  (newline)
  
  (display "  Rational 1/4 + 1/4 = 1/2: ")
  (display (equal? (+ 1/4 1/4) 1/2))
  (newline))
