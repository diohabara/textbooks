;; Tangent function
; Recursive k-term finite continued function
(define (cont-frac n d k)
  (define (frac i)
    (if (< i k)
        (/ (n i) (+ (d i) (frac (+ i 1))))
        (/ (n i) (d i))))
  (frac 1))
; Tangent function
(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
        x
        (- (square x))))
  (define (d k)
    (- (* 2 k) 1))
  (cont-frac n d k))
(define (square x) (* x x))

(tan-cf (asin (/ 1 (sqrt 2))) 10) ; 0.9999999999999997
(tan-cf (acos 1) 10) ; 0