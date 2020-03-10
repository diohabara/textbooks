;; Alternative procedural representation of pairs
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

;; for testing
(car (cons 55 33)) ; 55
(cdr (cons 55 33)) ; 33