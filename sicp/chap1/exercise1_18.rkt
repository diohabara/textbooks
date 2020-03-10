; Iterative Mutiplication
(define (double a) (+ a a))
(define (halve a) (/ a 2))
(define (* a b)
  (mul-iter a b 0))
(define (mul-iter a b product)
  (cond [(= b 0) product]
        [(even? b) (mul-iter (double a) (halve b) product)]
        [else (mul-iter a (- b 1) (+ a product))]))