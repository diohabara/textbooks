;; Simpson's Integration
; use 1.3.1's function
(define (cube x) (* x x x))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; integral
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; Simpson
(define (simpson f a b n)
  (define (inc x) (+ x 1))
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (mul k)
    (* (if (even? k) 2 4)
       (y k)))
  (* (/ h 3.0)
     (+ (y 0)
        (sum mul 1 inc (- n 1))
        (y n))))

; result of ordinary integral
(integral cube 0 1 0.01) ; 0.24998750000000042
(integral cube 0 1 0.001) ; 0.24998750000000042
; result of simpson integral
(simpson cube 0 1 100) ; 0.25
(simpson cube 0 1 1000) ; 0.25