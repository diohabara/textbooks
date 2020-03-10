;; 4.16
;; a:
; change lookup-variable-value to signal an error if the value it finds
; is the symbol *unassigned*
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (car vars))
             (if (eq? '*unassigned* (car vals))
                 (error "Unassigned Variable" var)
                 (car vals))]
            [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variable frame)
                (frame-values frame)))))
  (env-loop env))

;; b:
; scan-out-defines
; takes a procedure body and returns an equivalent one
; that has no internal definitions, by making the transformation described above
(define (scan-out-defines body)
  (define (make-variable-clauses body)
    (if (definition? (car body))
        (cons (list (definition-variable (car body))
                    '*unassigned*)
              (make-variable-clauses (cdr body)))
        '()))
  (define (definition-assignment-body body)
    (if (definition? (car body))
        (cons (make-assignment (definition-variable (car body))
                               (definition-value (car body)))
              (definition->assignment-body (cdr body)))
        body))
  (if (definition? (car body))
      (list (apply-in-underlying-scheme
             make-let
             (cons (make-variable-clauses body)
                   (definition->assignment-body body))))
      body))

;; c:
; install scan-out-defines either in make-procedure or in procedure-body
; which place is better?
; make-procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

; reason: if installed in procedure-body, it is called in apply and user-print
; but if installed in make-procedure, it is called only in eval

;; 4.17
; draw->skip

;; 4.18
; 
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (streamp-map f y))
  y)
; will this procedure work if internal definitions are scanned out as shown
; in this exercise?
(define solve
  (lambda (f y0 dt)
    (let ((y '*unassigned*)
          (dy '*unassigned*))
      (let ((a (integral (delay dy) y0 dt))
            (b (stream-map f y)))
        (set! y a)
        (set! dy b))
      y)))
; => first, y and dy is restricted
; next, a is restricted. even though dy is unassigned, the other things remained to be evaluated
; next, b is restricted. however, here y is not defined, so return error
; what if they are scanned out as shown in the text?
(define solve
  (lambda (f y0 dt)
    (let ((y '*unassigned*)
          (dy '*unassigned*))
      (set! y (integral (delay dy) y0 dt))
      (set! dy (stream-map f y))
      y)))
; => will wok

;; 4.19
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))
; ben
; b => 11
; a = 5
; => 16

; Alyssa
; => error

; Eva
; a => 5
; b => 15
; => 20

; Alyssa is correct

; 4.20
(define (f x)
  (letrec
      ((even? (lambda (n)
                (if (= n 0) #t (odd? (- n 1)))))
       (odd? (lambda (n)
               (if (= n 0) #f (even? (- n 1))))))
    <rest of body of f>))

; (letrec ((<var1> <exp1>) .... (<varn> <expn>))
;  <body>)
              
(letrec
    ((fact (lambda (n)
             (if (= n 1) 1 (* n (fact (- n 1)))))))
  (fact 10))

;; a:
; implement letrec as a derived expression
; by transforming a letrec expression into a let expression 
((letrec? exp) (eval (letrec->let exp) env))

(define (letrec? exp) (tagged-list? exp 'letrec))

(define (letrec->let exp)
  (let ((vars (map car (cadr exp)))
        (exps (map cdr (cadr exp)))
        (body (cddr exp)))
    (cons 'let
          (cons (map (lambda (x) (list x '*unassigned*)) vars)
                (append (map (lambda (x y) (cons 'set! (cosn x y))) vars exps)
                        body)))))

;; b:
; draw -> skip


;; 4.21
; can specify recursive procedures without using letrec, or define
((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
 10)

;; a:
; the procedure above is correct
; devise a procedure for computing Fibonacci number
((lambda (n)
   ((lambda (fib) (fib fib n))
    (lambda (fb k)
      (cond [(= k 0) 0]
            [(= k 1) 1]
            [else
             (+ (fb fb (- k 1))
                (fb fb (- k 2)))]))))
 10)

;; b:
(define (f x)
  (define (even? n)
    (if (= n 0) #t (ood? (- n 1))))
  (define (odd? n)
    (if (= n 0) #f (even? (- n 1))))
  (even? x))

; make an procedure wih neither internal definitions nor letrec
(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) #t (od? ev? od? (- n 1)))
   (lambda (ev? od? n)
     (if (= n 0) #f (ev? ev? od? (- n 1)))))))