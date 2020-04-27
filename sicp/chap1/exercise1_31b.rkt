;; Iterative Product
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        1
        (iter (next a) (* (term a) (result)))))
  (iter a 1))