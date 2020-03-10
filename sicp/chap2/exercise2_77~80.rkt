;; 2.77
; apply-generic is invoked twice for complex and rectangular

;; 2.78
; type-tag, contents, attach-tag from section 2.4.2
(define (attach-tag type-tag contents)
  (cons type-tag contents))

; extract datum
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
; extract contents
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))
; modify these procedures so that our generic system taks advantage of scheme's
; internal type system
(define (type-tag datum)
  (cond [(number? datum) 'scheme-number]
        [(pair? datum) (car datum)]
        [else (error "Bad tagged datum -- TYPE-TAG" datum)]))
(define (contents datum)
  (cond [(number? datum) datum]
        [(pair? datum) (cdr datum)]
        [else (error "Bad tagged datum -- TYPE-TAG" datum)]))
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

;; 2.79
; define generic predicate equ? that test the equality of two numbers
; for ordinary
(put 'equ? (scheme-number scheme-number)
     (lambda (x y) (= x y)))
; for rational
(define (equ-rat? x y)
  (and (= (numer x) (numer y)) (= (denom x) (denom y))))
(put 'equ? '(rational rational)
     (lambda (x y) (equ-rat? x y)))
; for complex
(define (equ-complex? x y)
  (and (= (real-part x) (real-part y))
       (= (imag-part x) (imag-part y))))
(put 'equ? '(complex complex)
     (lambda (x y) (equ-complex? x y)))
; for generic
(define (equ? x y) (apply-generic 'equ? x y))

;; 2.80
; define a generic predicate =zero? that tests if its argument is zero
; for ordinary
(put '=zero? (scheme-number)
     (lambda (x) (= x 0))) 
; for rational
(define (=zero-rat? x)
  (= (numer x) 0))
(put  '=zero? '(rational)
     (lambda (x) (=zero-rat? x)))
; for complex
(define (zero-complex? x)
  (and (= (real-part x) 0)
       (= (imag-part x) 0)))
(put '=zero? '(complex)
     (lambda (x) (zero-complex? x)))
; for generic
(define (=zero? x) (apply-generic '=zero? x)) 