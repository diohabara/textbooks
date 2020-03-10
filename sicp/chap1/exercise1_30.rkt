;; Iterative Summing
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        0
        (iter (next a) (+ (term a) result)))) 
  (iter a 0))