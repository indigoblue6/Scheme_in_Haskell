;; 簡単なテスト
(display "Rational: ")
(/ 1 2)
(display "\nComplex: ")
(+ 1+2i 3+4i)
(display "\nTail recursion: ")
(define (factorial n acc)
  (if (<= n 1)
      acc
      (factorial (- n 1) (* n acc))))
(factorial 10 1)
(display "\n")
