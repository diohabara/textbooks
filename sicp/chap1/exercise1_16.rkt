; Iterative Exponentiation
; Logarithmic number of steps
(define (fast-expr b n)
  (expr-iter b n 1))
(define (expr-iter b n product)
  (cond [(= n 0) product]
        [(even? n) (expr-iter (square b) (/ n 2) product)]
        [else (expr-iter b (- n 1) (* b product))]))
(define (square x) (* x x))