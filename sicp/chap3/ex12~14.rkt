;; 3.12
; append: appends two lists
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

; append!: modify the final pair of x so that its cdr is nwo y
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

; last-pair: return the last pair
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; for testing
(display "3.12\n")
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y)) ; (a b c d)
z ; {mcons 'a {mcons 'b {mcons 'c {mcons 'd '()}}}}
(cdr x) ; {mcons 'b '()}
(define w (append! x y))
w ; {mcons 'a {mcons 'b {mcons 'c {mcons 'd '()}}}}
(cdr x) ; {mcons 'b {mcons 'c {mcons 'd '()}}}

;; 3.13
; make-cycle:
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
; for testing
(display "3.13\n")
(define z (make-cycle (list 'a 'b 'c))) ; the last pointer is the one leading to the first element
z
;(last-pair z)
; infinite loop

;; 3.14
; temp: the cdr of x
; set-cdr! on the next line destroys the cdr
; mystery: reverse its argument, x
; first y will the 
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

; for testing
(display "3.14\n")
(define v (list 'a 'b 'c 'd))
v ; {mcons 'a {mcons 'b {mcons 'c {mcons 'd '()}}}}
(define w (mystery v))
w ; {mcons 'd {mcons 'c {mcons 'b {mcons 'a '()}}}}