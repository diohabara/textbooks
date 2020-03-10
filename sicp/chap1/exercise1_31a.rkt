; Sum Procedure
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; Product Procedure
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; John Wallis' formula
(define (wallis a b)
  (define (square x) (* x x))
  (define (mul k)
    (/
     (* (* 2 k) (* 2 (+ k 1)))
     (square (+ (* 2 k) 1))))
  (define (inc x) (+ x 1))
  (product mul a inc b))

; pi
(* 4.0 (wallis 1 100)) ; 3.1493784731686008
(* 4.0 (wallis 1 1000)) ; 3.142377365093878