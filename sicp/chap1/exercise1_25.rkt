; Square Exponetiation
(define (fast-expt b n)
  (cond [(= n 0) 1]
        [(even? n) (square (fast-expt b (/ n 2)))]
        [else (* b (fast-expt b (- n 1)))]))
(define (square x) (* x x))

; Alyssa P. Hacker's expmod
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;; In this procedure, one may need to use a larger integer.
;; That is because fast-expt doesn't need m.
;; It may take much longer time for calculating.