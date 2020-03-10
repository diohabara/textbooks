;; Euler e-2 expansion
; Recursive k-term finite continued fraction
(define (cont-frac n d k)
  (define (frac i)
    (if (< i k)
        (/ (n i) (+ (d i) (frac (+ i 1))))
        (/ (n i) (d i))))
  (frac 1))
; Napier's constant using euler e-2 expansion
(+ 2
   (cont-frac (lambda (i) 1.0)
              (lambda (i) (cond [(< i 3) i]
                                [(= (remainder i 3) 2) (* 2 (+ (quotient i 3) 1))]
                                [else 1]))
              100))