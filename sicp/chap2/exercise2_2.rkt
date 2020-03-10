;; Constructor and Selector of points
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

;; Constructor and Selector of segment
(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))
   
(define (midpoint-segment segment)
  (let ((start-point (car segment))
        (end-point (cdr segment)))
    (make-point (/ (- (x-point end-point)
                      (x-point start-point))
                   2)
                (/ (- (y-point end-point)
                      (y-point start-point))
                   2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; for tests
(define a (make-point 0 0))
(define b (make-point 3 3))
(define a-b (make-segment a b))
(print-point (midpoint-segment a-b)) ; (3/2,3/2)