;; 4.11
; each binding is a name-value pair
; rewrite an environment for this alternative representation
(define (make-frame variables values)
  (define (make-frame-iter variables values)
    (if (null? variables)
        '()
        (cons (cons (car variables)
                    (car values))
              (make-frame-iter (cdr varaibles)
                               (cdr values)))))
  (make-frame-iter variables values))

(definee (frame-variables frame)
  (if (null? frame)
      '()
      (cons (caar frame)
            (frame-variable (cdr frame)))))

(define (frame-values frame)
  (if (null? frame)
      '()
      (cons (cdar frame)
            (frame-value (cdr frame)))))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

;; 4.12
; find common pattern of set-variable-value!, define-variable! and
; lookup-variable-value
; and implement it more abstractly
; abstracted scan
(define (scan var vars vals)
  (cond [(null? vars) '()]
        [(eq? var (car vars)) vals]
        [else
         (scan var (cdr vars) (cdr vals))]))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env))
              (let ((result-of-scan (scan (var (frame-variables frame) (frame-values frame))))
                    (if (null? result-of-scan)
                        (env-loop (enclosing-environment env))
                        (car result-of-sanf))))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound varaible -- SET!" var)
        (let ((frame (first-frame env))
              (let ((result-of-scan (scan var (frame-variables frame) (frame-values frame))))
                (env-loop (enclosing-environment env))
                (set-car! result-of-scan val))))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((result-of-scan (scan var (frame-variables frame) (frame-values frame))))
      (if (null? result-of-scan)
          (add-binding-to-frame! var val frame)
          (set-car! result-of-scan val)))))


;; 4.13
; procedure that get rid of bindings
; remove bindings from the first frame

(define (unbind? exp) (tagged-list exp 'unbind!))

(define (eval-unbind exp env)
  (unbind-variable! (unbind-variable exp) env)
  'ok)

(define (unbind-variable exp) (cadr exp))

(define (unbind-variable! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond [(null? vars)
             (error "Unbound varible --UNBIND-VARIABLE:" var)]
            [(eq? var (car vars))
             (set-car! vars (cadr vars))
             (set-cdr! vars (cddr vars))
             (set-car! vals (cadr vals))
             (set-cdr! vals (cddr vals))]
            [else (scan (cdr vars) (cdr vals))]))
    (scan (frame-variables frame)
          (frame-values frame))))
; put this into eval
[(unbind? exp) (eval-unbind exp env)]