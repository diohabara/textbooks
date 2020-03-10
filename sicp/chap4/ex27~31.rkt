;; 4.27
; just evaluate the following interactions
(define count 0)
(define (id x) (set! count (+ count 1)) x)
; -> other code

;; 4.28
; demonstrate the need for evaluation of the operator before passing it to apply
; rather actual-value than eval
; when evaluate procedure that as an argument takes a procedure will make an error
; because, every arguments are thunks

;; 4.29
; exhibit a program that you would expect to run more slowly without memoization
; => skip

; give the both reponse with and without memoization
; with memoization
(define (force-it obj)
  (cond [(thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result) ; set exp as obj
           (set-cdr! (cdr obj) '()) ; ignore inessential env
           result)]
        [(evaluated-thunk? obj)
         (thunk-value obj)]
        [else obj]))

; without memoization
(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

;; 4.30
; modified eval-sequence to use actual-value rather than eval
; original
(define (eval-sequence exps env)
  (cond [(last-exp? exps) (eval (first-exp exps) env)]
        [else (eval (first-exp epxs) env)
              (eval-sequence (rest-exps exps) env)]))

; Cy
(define (eval-sequence exps env)
  (cond [(last-exp? exps) (eval (first-exp exps) env)]
        [else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env)]))

;; a: important example of a sequence with side effects
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))
; just evaluate these expressions
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

; b:
; defined in the lazy evaluator
(define (p1 x)
  (set! x (cons x '(2)))
  x)
(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))
; what are the values of (p1 1) and (p2 1)
; with the original eval-sequence
; -> at p1, (set! x (cons x `(2))) is also evaluated
; and at p2, since p is not primitive procedure, the qrgument
; (set! x (cons x `(a))) is delayed and not evaluated
; with the lazy eval-sequence
; ->each of p1 and p2 is forced and evaluated
; so (set! x (cons x `(2))) is also evaluated

; c:
; why does changing eval-sequence not affect the behavior of the example in part a,
; reason: in part a, expressions on process are used and not delayed
; eval-sequence, force expressions be used and not delayed

; d: how do sequences ought to be treated?
; I prefer normal-order.
; because it returns value only when it is needed.

;; 4.31
; difficult...
; skip