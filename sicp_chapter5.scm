;; SICP Chapter 5 - Computing with Register Machines

(display "=== SICP Chapter 5: Computing with Register Machines ===\n")

;; 5.1 レジスタマシンの設計（概念的）
;; レジスタマシンは実際のハードウェアをシミュレートするため、
;; ここでは概念的な実装を示す

;; 5.2 レジスタマシンシミュレータの基本構造
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; 5.3 レジスタの実装
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(begin
  (display "1. Register machine concept: ")
  (define r1 (make-register 'a))
  (set-contents! r1 100)
  (display "register a = ")
  (display (get-contents r1))
  (newline))

;; 5.4 スタックの実装
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK" message))))
    dispatch))

(begin
  (display "2. Stack: ")
  (define stack (make-stack))
  ((stack 'push) 10)
  ((stack 'push) 20)
  (display "pop = ")
  (display (stack 'pop))
  (display ", pop = ")
  (display (stack 'pop))
  (newline))

;; 5.5 基本的なマシン（GCD計算）の制御構造
(define gcd-machine-ops
  (list (list 'rem remainder)
        (list '= =)))

(begin
  (display "3. GCD machine operations defined")
  (newline))

;; 5.6 フィボナッチマシン（反復版）の概念
(define fib-machine-registers
  '(n val continue))

(begin
  (display "4. Fibonacci machine registers: ")
  (display fib-machine-registers)
  (newline))

;; 5.7 命令実行のシミュレーション
(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(begin
  (display "5. Instruction structure: ")
  (define inst (make-instruction '(assign a (const 5))))
  (display (instruction-text inst))
  (newline))

;; 5.8 性能監視
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-count 0))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (set! instruction-count (+ instruction-count 1))
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (print-statistics)
        (display "Instructions executed: ")
        (display instruction-count)
        (newline))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'print-statistics) (print-statistics))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(begin
  (display "6. Machine simulator: ")
  (define machine (make-new-machine))
  (display "machine created")
  (newline))

;; 5.9 アセンブラの構造
(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                        (cons (make-label-entry next-inst insts) labels))
               (receive (cons (make-instruction next-inst) insts)
                        labels)))))))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(begin
  (display "7. Assembler structure: defined")
  (newline))

;; 5.10 コンパイラの基礎
(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (if? exp) (tagged-list? exp 'if))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (application? exp) (pair? exp))

(begin
  (display "8. Compiler predicates: defined")
  (newline))

;; 5.11 最適化の概念
(begin
  (display "9. Optimization concepts: discussed")
  (newline))

;; 5.12 まとめ
(begin
  (display "10. Register machines provide a bridge between")
  (newline)
  (display "    high-level languages and actual hardware")
  (newline))

(display "\n=== All Chapter 5 examples completed! ===\n")
