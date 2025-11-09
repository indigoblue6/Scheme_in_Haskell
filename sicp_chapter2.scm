;; SICP Chapter 2 - Data Abstraction Examples

(display "=== SICP Chapter 2: Data Abstraction ===\n")

;; 2.1 有理数演算
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

(begin
  (display "1. Rational numbers: ")
  (define one-half (make-rat 1 2))
  (define one-third (make-rat 1 3))
  (print-rat (add-rat one-half one-third))
  (display " = 5/6")
  (newline))

;; 2.2 線分の表現
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (make-point (/ (+ (x-point start) (x-point end)) 2)
                (/ (+ (y-point start) (y-point end)) 2))))

(begin
  (display "2. Line segments: midpoint of (0,0)-(4,4) = ")
  (define seg (make-segment (make-point 0 0) (make-point 4 4)))
  (define mid (midpoint-segment seg))
  (display "(")
  (display (x-point mid))
  (display ",")
  (display (y-point mid))
  (display ")")
  (newline))

;; 2.3 リスト操作
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length-iter items)
  (define (iter lst count)
    (if (null? lst)
        count
        (iter (cdr lst) (+ count 1))))
  (iter items 0))

(define (append-lists list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append-lists (cdr list1) list2))))

(begin
  (display "3. List operations: ")
  (display "length of (1 2 3 4 5) = ")
  (display (length-iter '(1 2 3 4 5)))
  (display ", ")
  (display "3rd element = ")
  (display (list-ref '(1 2 3 4 5) 2))
  (newline))

;; 2.4 map
(define (map-proc proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map-proc proc (cdr items)))))

(define (scale-list items factor)
  (map-proc (lambda (x) (* x factor)) items))

(begin
  (display "4. Map: scale (1 2 3 4 5) by 10 = ")
  (display (scale-list '(1 2 3 4 5) 10))
  (newline))

;; 2.5 for-each
(define (for-each-proc proc items)
  (cond ((null? items) #t)
        (else (begin
                (proc (car items))
                (for-each-proc proc (cdr items))))))

(begin
  (display "5. For-each: ")
  (for-each-proc (lambda (x) (begin (display x) (display " ")))
                 '(1 2 3 4 5))
  (newline))

;; 2.6 木構造
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(begin
  (display "6. Tree operations: leaves in ((1 2) (3 4)) = ")
  (display (count-leaves '((1 2) (3 4))))
  (newline))

;; 2.7 map with multiple lists
(define (map-multiple proc . lists)
  (if (null? (car lists))
      '()
      (cons (apply proc (map-proc car lists))
            (apply map-multiple (cons proc (map-proc cdr lists))))))

(begin
  (display "7. Map with multiple lists: (+ 1 2 3) (+ 4 5 6) = ")
  (display (map-multiple + '(1 4) '(2 5) '(3 6)))
  (newline))

;; 2.8 フィルター
(define (filter-items predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter-items predicate (cdr sequence))))
        (else (filter-items predicate (cdr sequence)))))

(begin
  (display "8. Filter: odd numbers in (1 2 3 4 5 6 7 8) = ")
  (display (filter-items odd? '(1 2 3 4 5 6 7 8)))
  (newline))

;; 2.9 畳み込み (fold-right / accumulate)
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(begin
  (display "9. Fold operations:\n")
  (display "   fold-right - (1 2 3 4) = ")
  (display (fold-right - 0 '(1 2 3 4)))
  (newline)
  (display "   fold-left - (1 2 3 4) = ")
  (display (fold-left - 0 '(1 2 3 4)))
  (newline))

;; 2.10 flatmap
(define (flatmap proc seq)
  (fold-right append '() (map-proc proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(begin
  (display "10. Flatmap: enumerate 1 to 3 twice = ")
  (display (flatmap (lambda (i) (enumerate-interval 1 i))
                    '(1 2 3)))
  (newline))

;; 2.11 素数ペア
(define (prime? n)
  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (cond ((> (* test-divisor test-divisor) n) n)
            ((= (remainder n test-divisor) 0) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))))
    (find-divisor n 2))
  (and (> n 1) (= n (smallest-divisor n))))

(define (prime-sum-pairs n)
  (define (prime-sum? pair)
    (prime? (+ (car pair) (car (cdr pair)))))
  (define (make-pair-sum pair)
    (list (car pair) (car (cdr pair)) (+ (car pair) (car (cdr pair)))))
  (map-proc make-pair-sum
            (filter-items prime-sum?
                         (flatmap
                          (lambda (i)
                            (map-proc (lambda (j) (list i j))
                                     (enumerate-interval 1 (- i 1))))
                          (enumerate-interval 1 n)))))

(begin
  (display "11. Prime sum pairs (n=6): ")
  (display (prime-sum-pairs 6))
  (newline))

;; 2.12 集合演算（リスト表現）
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(begin
  (display "12. Set operations: ")
  (define s1 '(1 2 3 4))
  (define s2 '(3 4 5 6))
  (display "intersection of (1 2 3 4) and (3 4 5 6) = ")
  (display (intersection-set s1 s2))
  (newline))

(display "\n=== All Chapter 2 examples completed! ===\n")
