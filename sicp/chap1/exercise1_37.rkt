;; k-term finite continued fraction
; Expected value is the same as 0.16180 to 4 decimal places
; Recursive Version
(define (cont-frac n d k)
  (define (frac i)
    (if (< i k)
        (/ (n i) (+ (d i) (frac (+ i 1))))
        (/ (n i) (d i))))
  (frac 1))
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10) ; 0.6179775280898876

; Iterative Version
(define (iter-cont-frac n d k)
  (define (iter-frac i result)
    (if (= i 0)
        result
        (iter-frac (- i 1) (/ (n i) (+ (d i) result)))))
    (iter-frac (- k 1) (/ (n k) (d k))))
(iter-cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                10) ; 0.6179775280898876