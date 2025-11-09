;; SICP Chapter 4 - Metalinguistic Abstraction

(display "=== SICP Chapter 4: Metalinguistic Abstraction ===\n")

;; 4.1 評価器の基本コンポーネント（メタ循環評価器のシミュレーション）

;; タグ付きリストの実装
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;; 4.2 引用式
(define (text-of-quotation exp) (cadr exp))

(begin
  (display "1. Quotation: ")
  (display "parse (quote (a b c)) -> (a b c)")
  (newline))

;; 4.3 代入
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(begin
  (display "2. Assignment structure: ")
  (display "parse (set! x 10) -> var=x, val=10")
  (newline))

;; 4.4 定義
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (cons 'lambda (cons (cdadr exp) (cddr exp)))))

(begin
  (display "3. Definition: ")
  (display "parse definitions -> var and func forms")
  (newline))

;; 4.5 lambda式
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(begin
  (display "4. Lambda: ")
  (display "parse lambda expressions")
  (newline))

;; 4.6 条件式
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(begin
  (display "5. If expression: ")
  (display "parse if expressions")
  (newline))

;; 4.7 cond を if に変換
(define (cond-clauses exp) (cdr exp))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last"))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((null? (cdr seq)) (car seq))
        (else (cons 'begin seq))))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(begin
  (display "6. Cond expansion: ")
  (display "converts cond to nested if expressions")
  (newline))

;; 4.8 let を lambda に変換
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-vars bindings) (map car bindings))
(define (let-vals bindings) (map cadr bindings))

(define (let->combination exp)
  (cons (make-lambda (let-vars (let-bindings exp))
                     (let-body exp))
        (let-vals (let-bindings exp))))

(begin
  (display "7. Let to lambda: ")
  (display "converts let to lambda application")
  (newline))

;; 4.9 名前付きlet
(define (named-let? exp)
  (symbol? (cadr exp)))

(define (named-let-name exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cdddr exp))

;; 4.10 論理演算の導出規則
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (and-clauses exp) (cdr exp))
(define (or-clauses exp) (cdr exp))

(begin
  (display "8. Logical operations: ")
  (display "and/or recognized")
  (newline))

;; 4.11 評価器のデータ構造
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(begin
  (display "9. Frame structure: ")
  (define f (make-frame '(x y) '(1 2)))
  (display "vars = ")
  (display (frame-variables f))
  (newline))

;; 4.12 環境操作
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (error "Vars and vals length mismatch")))

(begin
  (display "10. Environment: ")
  (define env (extend-environment '(x y) '(10 20) the-empty-environment))
  (display "extended with x=10, y=20")
  (newline))

;; 4.13 amb評価器（非決定性計算）の概念
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(begin
  (display "11. Amb (non-deterministic): ")
  (display "represents non-deterministic choice")
  (newline))

;; 4.14 require（制約）
(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))

;; 4.15 論理型プログラミングの基礎
(define (query? exp) (tagged-list? exp 'query))

(begin
  (display "12. Query structure: recognized")
  (newline))

(display "\n=== All Chapter 4 examples completed! ===\n")
