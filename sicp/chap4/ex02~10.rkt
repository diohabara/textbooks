;; 4.2
;; a: What is wrong with Louis's plan?
; assignment expressions will be evaluated as applications
; it in turn lead evaluator to evaluate the assignment variable
; instead of treating it as a symbol

;; b: change syntax of the evaluated language so that procedure applications
; start with 'call
(define (eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(quoted? exp) (text-of-quotation exp)]
        [(application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env))]
        [(assignment? exp) (eval-assignment exp env)]
        [(definition? exp) (eval-definition exp env)]
        [(if? exp) (eval-if exp env)]
        [(lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env)]
        [(begin? exp)
         (eval-sequence (begin-actions exp) env)]
        [(cond? exp) (eval (cond->if exp) env)]
        [else
         (error "Unknown expression type: EVAL" exp)]))

(define (applicatioin? exp) (tagged-list exp 'call))

(define (operator exp) (cadr exp))

(define (operands exp) (cddr exp))

;; 4.3
; rewrite eval so that the dispatch is done in data-directed style

(define operation-table make-table)
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc))

(put 'eval 'quote text-of-quotation)
(put 'eval 'set! eval-assignment)
(put 'eval 'define eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda (lambda (exp env)
                   (make-procedure (lambda-parameters exp) (lambda-body env) env)))
(put 'eval 'begin (lambda (exp env)
                  (eval-sequence (begin-sequence exp) env)))
(put 'eval 'cond (lambda (exp env)
                 (eval (cond-if exp) env)))

(define (eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(get 'eval (car exp)) (get 'eval (car exp) exp env)]
        [(application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env))]
        [else (error "Unknown expression type -- EVAL" exp)]))

;; 4.4
; define 'and' & 'or' procedure
#| my implementation
; common
(define (empty-exp? exp) (null? exp))

(define (first-exp exp) (car exp))

(define (rest-exp exp) (cdr exp))

; and
(define (eval-and exp env)
  (cond [(empty-exp? exp) #t]
        [else
         (let ((first (eval (first-exp exp) env))
               (rest (rest-exp exp)))
           (cond [(empty-exp? rest) first]
                 [(true? first) (eval-and rest env)]
                 [else #f]))]))

; or
(define (eval-or exp env)
  (cond [(empty-exp? exp) #f]
        [else
         (let ((first (eval (first-exp exp) env))
               (rest (rest-exp exp)))
           (cond [(empty-exp? rest) first]
                 [(false? first) (eval-or rest env)]
                 [else #t]))]))
|#

; not mine
(define (eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(quoted? exp) (text-of-quotation exp)]
        [(assignment? exp) (eval-assignment exp env)]
        [(definition? exp) (eval-definition exp env)]
        [(and? exp) (eval-and (and-clauses exp) env)]
        [(or? exp) (eval-or (or-vlauses exp) env)]
        [(if? exp) (eval-if exp env)]
        [(lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env)]
        [(begin? exp)
         (eval-sequence (begin-actions exp) env)]
        [(cond? exp) (eval (cond->if exp) env)]
        [(application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env))]
        [else
         (error "Unknown expression type: EVAL" exp)]))

; and
(define (and? exp) (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))
(define (and-first-exp exp) (car exp))
(define (and-rest-exps exp) (cdr exp))
(define (eval-and exp env)
  (define (eval-and-iter exp result)
    (if (null? exp)
        result
        (let ((first-eval (eval (and-first exp exp) env))
              (rest (and-rest-exps exp)))
          (if (true? first-eval)
              (eval-and-iter rest first-eval)
              'false))))
  (if (null? exp)
      'true
      (eval-and-iter exp '())))

; or
(define (or? exp) (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))
(define (or-first-exp exp) (car exp))
(define (or-rest-exps exp) (cdr exp))
(define (eval-or exp env)
  (if (null? exp)
      'false
      (let ((first-eval (eval (or-first-exp exp) env))
            (rest (or-rest-exp exp)))
        (if (true? first-eval)
            first-eval
            (eval-or rest env)))))

;; derived one
(define (and? exp) (eval (and-if exp) env))
(define (or? exp) (eval (or-if exp) env))

; and
(define (and? exp) (tagged-list exp 'and))
(define (and-clauses exp) (cdr exp))
(define (and-first-exp exp) (car exp))
(define (and-rest-exps exp) (cdr exp))
(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))
(define (expand-and-clauses clauses)
  (define (expand-and-iter clauses result)
    (if (null? clauses)
        result
        (let ((first (and-first-exp clauses))
              (rest (and-rest-exps clauses)))
          (make-if first
                   (expand-and-iter rest first)
                   'false))))
  (if (null? clauses)
      'true
      (expand-and-iter clauses '())))

; or
(define (or? exp) (tagged-list? exp 'or))
(define (or-clausese exp) (cdr exp))
(define (or-first-exp exp) (car exp))
(define (or-rest-exps exp) (cdr exp))
(define (or->if exp)
  (expand-or-clausese (or-clauses exp)))
(define (expand-or-clauses clauses)
  (if (null? clauses)
      'flase
      (let ((first (or-first-exp clauses))
            (rest (or-rest-exps clauses)))
        (make-if first
                 first
                 (expand-or-clauses rest)))))

;; 4.5
; additional syntax for cond clauses
(define (expand-clauses clauses)
  (if (null? clauses)
      #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clauese? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clauses isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (let ((action (cond-actions first))
                           (predicate (cond-predicate first)))
                       (if (eq? (car action) '=>)
                           (list (cadr action) predicate)
                           (sequence->exp action)))
                     (expand->clauses rest))))))

;; 4.6
; syntactic transformation let->combination
(define (let? exp) (tagged-list? exp 'let))
(define (let-clauses exp) (cdr exp))
(define (let-bindings clauses) (car clauses))
(define (let-body clauses) (cdr clauses))
(define (let->combination exp)
  (expand-let-clauses (let-clauses exp)))
(define (expand-let-clauses clauses)
  (if (null? (let-bindings clauses))
      '()
      (cons (make-lambda (map car (let-bindings clauses)) (let-body clauses))
            (map cadr (let-bindings clauses)))))

;; 4.7
(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
  (* x z))
; is equal to
(let ((x 3))
  (let ((y (+ x 2)))
    (let ((z (+ x y 5)))
      (* x z))))

; let*->nested-lets

; evaluator to handle let*
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-clauses exp) (cdr exp))
(define (let*-bindings clauses) (car clauses))
(define (let*-body clauses) (cadr clauses))
(define (make-let* defs body)
  (list 'let defs body))
(define (let*->nested-lets exp)
  (if (null? exp)
      'false
      (let ((clauses (let*-clauses exp)))
        (let ((bindings (let*-bindings clauses))
              (body (let*-body clauses)))
          (define (iter rest-bindings)
            (if (null? rest-bindings)
                body
                (make-let* (list (car rest-bindings))
                           (iter (cdr rest-bindings)))))
          (iter bindings)))))

;; 4.8
; named let
; using named let, fibonacci
(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))
; which is equal to
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))
; modify let->combination to support named let
; using some procedure from ex4.6
(define (let->combination exp)
  (if (pair? (car (let-clauses exp)))
      (expand-let-clauses (let-clauses exp))
      (expand-named-let-clauses (let-clauses exp))))
(define (named-let-var claueses) (car claues))
(define (named-let-bindings clauses) (cadr clauses))
(define (name-let-body clauses) (caddr clauses))
(define (expand-named-let-clauses clauses)
  (make-begin
   (list
    (list 'define (cons (name-let-var claueses)
                        (map car (named-let-bindings clauses)))
          (named-let-body clauses))
    (cons (named-let-var clauses)
          (map cadr (name-let-bindings clauses))))))

;; 4.9
; implement iteration
(define (while? exp) (tagged-list? exp 'while))
(define (while-predicate exp) (cadr exp))
(define (while-body exp) (cddr exp))
(define (while->let exp)
  (let ((predicate (while-predicate exp))
        (body (while-body exp)))
    (list 'let 'while-loop '()
          (make-if predicate
                   (append (cons 'begin body)
                           (list (cons 'while-loop '())))
                   'true))))

;; 4.10
; by using data abstraction, we can write an eval procedure
; that is independent of the particular syntax withoud changing eval, apply
; namely, introducing another syntax
; (// a b) -> (quotient a b)
; -> failed...
; someone's postfix notation
(define (last-element lst)
  (if (null/ (cdr lst))
      (car lst)
      (last-element (cdr lst))))
(define (tagged-list? exp sym)
  (if (pair? exp)
      (let ((last (last-element exp)))
        (eq? last sym))
      #f))
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (car exp))
(define (if-consequent exp) (cadr exp))
(define (if-alternative exp)
  (if (= (length exp) 4)
      (caddr exp)
      'false))