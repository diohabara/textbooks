;; 4.1
; list-of-values that evaluates operands from left to right
; regardless of underlying lisp
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-eval (eval (first-oprands exps) env)))
        (cons first-eval
              (list-of-values (rest-oeprands exps) env)))))

; list-of-value from right to left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-eval (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operands exps) env)
              first-eval))))