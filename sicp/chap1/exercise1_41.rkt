(define (inc x) (+ x 1))
(define (double procedure)
  (lambda (x) (procedure (procedure x))))
((double inc) 1) ; 3
(((double (double double)) inc) 5) ; 21