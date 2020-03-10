;; 3.10
; make-withdraw: creates local state variable explicitly using let
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
; (let ((<variable> <expression>)) <body>) is the syntax sugar of 
; ((lambda (<variable> <body>) <expression>)
(define W1 (make-withdraw 100))
(W1 50) ; 50
(define W2 (make-withdraw 100))
(W2 0) ; 100
; here pictures are required, so skip