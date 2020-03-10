;; Pairs of nonnegative integers using oly numbers and arithmatic operations
(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car x)
  (if (= (remainder x 2) 0)
      (+ 1 (car (/ x 2)))
      0))

(define (cdr x)
  (if (= (remainder x 3) 0)
      (+ 1 (cdr (/ x 3)))
      0))