;; Church numerals
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; Expriment
(define (inc x) (+ x 1))
(define (square x) (* x x))
(display "zero expriment\n")
((zero inc) 1) ; 1 
((zero inc) 5) ; 5
((zero square) 1) ; 1
((zero square) 5) ; 5
(newline)

(display "add-1 zero expriment\n")
(((add-1 zero) inc) 1) ; 2
(((add-1 zero) inc) 5) ; 6
(((add-1 zero) square) 1) ; 1
(((add-1 zero) square) 5) ; 25
(newline)
(add-1 zero)
(lambda (f) (lambda (x) (f (lambda (g) (lambda (y) y)) f x)))
(lambda (f) (lambda (x) (f (lambda (g)

(display "double add-1 zero expriment\n")
(((add-1 (add-1 zero)) inc) 1) ; 3 
(((add-1 (add-1 zero)) inc) 5) ; 7
(((add-1 (add-1 zero)) square) 1) ; 1
(((add-1 (add-1 zero)) square) 2) ; 16
(((add-1 (add-1 zero)) square) 5) ; 625

;; define one, two
(define (one f)
  (lambda (f) (lambda (x) (f x))))
(define (two f)
  (lambda (f) (lambda (x) (f (f x)))))
