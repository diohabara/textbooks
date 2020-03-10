; Sum Procedure
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (product term (next a) next b))))

; Product Procedure
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; Recursive Accumulation Procedure
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value (term a) next b))))