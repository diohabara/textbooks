;; 3.77
; integral that is more like integers-starting-frmo
(define (integral integrand initial-value dt)
  (cons-stream
   initial-value
   (if (stream-null? integrand)
       the-empty-stream
       (integral (stream-cdr integrand)
                 (+ (* dt (stream-car integrand))
                    initial-value)
                 dt))))

; modify this so that it expects the integrand as a delayed argument
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

;; 3.78
; solve-2nd: takes as arguments the constants a, b, and dt
; and the initial value y0 and dy0 for y and dy/dt
; and generates the stream of successive values of y
(define (solve-2nd a b dt y0 dy0)
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  (define dy (integral (delay ddy) dy0 dy))
  (define y (integral (delay dy) y0 dt))
  y)

;; 3.79
; generalize the solve-2nd procedure
; so that it can be used to solve general second-order differential equations
(define (generalized-solve-solve f dy y0 dy0)
  (define ddy (stream-map f dy y))
  (define dy (integral (delay ddy) dy0 dt))
  (define y (integral (delay dy) y0 dt))
  y)

;; 3.80
; write a procedure RLC that takes as arguments the parameters R, L, and C
; of the circuit and the time increment dt
; RLC should produce a procedure that takes the initial values of the
; state variables vc0 and il0 nad produces a pair of streams of state vc and il
(define (RLC R L C dt)
  (define (rlc vC0 iL0)
    (define dvC (scale-steram iL (/ -1 C)))
    (define diL (add-streams (scale-stream vC (/ 1 L))
                             (scale-stream iL (- (/ R L)))))
    (define iL (integral (delay diL) iL0 dt))
    (define vC (integral (delay dvC) vC0 dt))
    (stream-map (lambda (i v) (cons i v)) iL vC))
  rlc)
; using RLC, whose R = 1, C = 0.2, L= 1, dt = 0.1, il0 = 0, vc0 = 10
; make a pair of streams vc0 and il0
(define RLC1 (RLC 1 1 0.2 0.1))
(define rlc-pair (define 0 10))