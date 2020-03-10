;;; Codes from previous exercise
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

;; Constructor and Selector of regtangle
(define (make-rect height-seg width-seg)
  (cons height-seg width-seg))
(define (rect-height rect)
  (car rect))
(define (rect-width rect)
  (cdr rect))

(define (peri rect)
  (* (+ (car rect) (cdr rect)) 2))

(define (area rect)
  (* (car rect) (cdr rect)))

(define (square x) (* x x))

(define (length segment)
  (let ((dx (- (x-point (end-segment segment))
               (x-point (start-segment segment))))
        (dy (- (y-point (end-segment segment))
               (y-point (start-segment segment))))))
  (sqrt (+ (square dx) (square dy))))

(define (height rect)
  (length (rect-height rect)))
(define (width rect)
  (length (rect-width rect)))

;; for testing
(define rect (make-rect (make-segment (make-point 1 1)
				      (make-point 3 1))
			(make-segment (make-point 1 1)
                                      (make-point 1 5))))
(area rect) ; 8
(peri rect) ; 12