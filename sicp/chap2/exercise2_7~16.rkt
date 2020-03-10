;; 2.1.4 Extended Exercise: Internval Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

;; Exercise2.7
; define constructor and selector
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

;; Exercise2.8
; subtraction procedure
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y)
                    (upper-bound x) (upper-bound y))))

;; Exercise2.9
(define (show-width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))
(display "Exercise2.9\n")
; these procedures have the same intervals
(show-width (add-interval
             (make-interval 10 20)
             (make-interval 30 50))) ; 15
(show-width (add-interval
             (make-interval 0 10)
             (make-interval 20 40))) ; 15
; these do not, even though these widths are the same, which means it is not a function
(show-width (mul-interval
             (make-interval 10 20)
             (make-interval 30 50))) ; 350
(show-width (mul-interval
             (make-interval 0 10)
             (make-interval 20 40))) ; 200

;; Exercise2.10
; modify division so that it cannot divide by 0
(define (div-interval x y)
  (define (span-zeros? y)
    (and (>= (upper-bound y) 0)
         (<= (lower-bound y) 0)))
  (if (span-zeros? y)
      (error "Cannot be devided by 0!")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

;; Exercise2.11
; Ben suggestion model(but I am lazy enough to extract codes from
; https://github.com/kean/SICP/blob/master/Chapter%202/sicp-exercise-2-07%20-%202-16.scm
; since it is just tedious work...
(define (complex-mul-interval x y)
  (let ((a (lower-bound x))
	(b (upper-bound x))
	(c (lower-bound y))
	(d (upper-bound y)))
    (cond ((and (> a 0) (> b 0))
	   (cond ((and (> c 0) (> d 0))
		  (make-interval (* a c) (* b d)))
		 ((and (< c 0) (> d 0))
		  (make-interval (* b c) (* b d)))
		 ((and (< c 0) (< d 0))
		  (make-interval (* b d) (* a c)))))
	  ((and (< a 0) (> b 0))
	   (cond ((and (> c 0) (> d 0))
		  (make-interval (* a d) (* a c)))
		 ((and (< c 0) (> d 0))
		  (make-interval (min (* a d) (* b c))
				 (max (* a c) (* b d))))
		 ((and (< c 0) (< d 0))
		  (make-interval (* b d) (* a d)))))
	  ((and (< a 0) (< b 0))
	   (cond ((and (> c 0) (> d 0))
		  (make-interval (* b d) (* a c)))
		 ((and (< c 0) (> d 0))
		  (make-interval (* b d) (* b c)))
		 ((and (< c 0) (< d 0))
(make-interval (* a c) (* b d))))))))

;; Exercise2.12
; Here we have an assumption that percentage is 1 or smaller
; alternative constructor and selector
; textbook version
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; Precentage Version
(define (make-center-percent c p)
  (let ((w (* c p)))
    (make-center-width c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (percent i)
  (/ (width i) (center i)))

;; Exercise2.13
; skip

;; Exercise2.14
; two ways of writing parallel-resistors (gives different answers)
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))
(display "Exercise2.14\n")
(define r1 (make-center-percent 2 0.03))
(define r2 (make-center-percent 5 0.05))
(par1 r1 r2) ; {mcons 1.2606019151846786 1.616591928251121}
(par2 r1 r2) ; {mcons 1.3774289985052317 1.4794801641586868}

;; Exercise2.15
; true, par2 is better
; divide an interval by itself is just an approximation

;; Exercise2.16
; no, impossible
; source: https://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem