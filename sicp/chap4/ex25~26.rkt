;; 4.25
; suppose that (in ordinary applicative-order scheme)
; we define unless as shown above and the define factorial
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))
; what happens?
; will loop
; because scheme is applicative-order and, evaluate factorial in factorial in...
(factorial 5)

; will it work in normal-order?
; will evaluate properly and return 120

;; 4.26
; implement unless as a derived expression
((unless? exp) (analyze (unless->if exp)))

(define (unless? exp) (tagged-list? exp 'unless))

(define (unless-predicate exp) (cadr exp))

(define (unless-consequent exp) (caddr exp))

(define (unless-alternative exp)
  (if (not (null? (cddr exp)))
      (cadddr exp)
      #f))

(define (unless->if exp)
  (make-if (unless-predicate exp)
           (unless-alternative exp)
           (unless-consequent exp)))