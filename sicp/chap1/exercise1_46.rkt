; former fixed-point 
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

; former square root
(define (average a b) (/ (+ a b) 2))
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;; good-enough?: a method for telling whether a guess is good enough
;; improve: a method for improving a guess
(define (iterative-improve good-enough? improve)
  (define (imp-iter guess)
    (if (good-enough? guess)
        guess
        (imp-iter (improve guess))))
  imp-iter)

; redefine sqrt
(define (sqrt x)
  ((iterative-improve (lambda (guess)
                        (< (abs (- (square guess) x))
                           0.001))
                      (lambda (guess)
                        (average guess (/ x guess))))
   1.0))

; redefine
(define (fixed-point f first-guess)
  (define tolerance 0.000001)
  ((iterative-improve (lambda (guess)
                        (< (abs (- (f guess) guess))
                           tolerance))
                      (lambda (guess)
                        (f guess)))
   (firs-guess)))