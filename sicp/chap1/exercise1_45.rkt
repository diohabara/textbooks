; finding fixed points
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; compose two functions together
(define (compose f g)
  (lambda (x) (f (g x))))
; repeat mapping n times
(define (repeated f n)
  (define (repeat-iter g cnt)
    (if (>= cnt n)
        g
        (repeat-iter (compose g f) (+ cnt 1))))
  (repeat-iter f 1))

;; Procedures as Returned Values
; average damping
(define (average a b) (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

;; nth power of x
(define (power x n)
  (if (= n 1)
      x
      (* x (power x (- n 1)))))

(define (nth-root x nth)
  (fixed-point
   ((repeated average-damp (floor (log nth 2)))
    (lambda (y)
      (/ x (power y (- nth 1)))))
   1.0))

(nth-root 8 3) ; 1.9999981824788517