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
; derivative procedure
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)
; cube
(define (cube x) (* x x x))
; square
(define (square x) (* x x))
;Newton's method as a fixed-point process
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))
; the root of x^3 + 2x^2 + x + 2 = 0
; is x = -2
(newtons-method (cubic 2 1 2) 1) ; -1.9999999999806546