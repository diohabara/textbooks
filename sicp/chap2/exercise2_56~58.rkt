;; 2.56
; differentiation rules
(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp) (if (same-variable? exp var) 1 0)]
        [(sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))]
        [else
         (error "unknown expression type: DERIV" exp)]))

; representing algebraic expression
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2))
         (+ a1 a2)]
        [else (list '+ a1 a2)]))
(define (=number? exp num) (and (number? exp) (= exp num)))

; better make-product: simplify the result
(define (make-product m1 m2)
  (cond [(or (=number? m1  0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list '* m1 m2)]))

; define exponentiation differentiation
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation base exponent)
  (cond [(=number? exponent 0) 1]
        [(=number? exponent 1) base]
        [(and (number? base) (number? exponent)) (expt base exponent)]
        [else (list '** base exponent)]))
(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [(sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))]
        [(exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation
                         (base exp)
                         (make-sum (exponent exp) -1)))
          (deriv (base exp) var))]
        [else
         (error "unknown expression type -- DERIV" exp)]))

; for testing
(display "2.56\n")
(deriv '(** x 2) 'x)
(deriv '(* a (** x 2)) 'x)
(deriv '(+ (+ (* a (** x 2)) (* b x)) c) 'x)

;; 2.57
; redefine sums and products so that deriv can return an expression which
; has more than one argument
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

; for testing
(display "2.57\n")
(deriv '(* x y (+ x 3)) 'x)

;; 2.58s
; a: differentiate infix representations
; in infix representations, operators are in the middle,
; and addend/multiplier is the first, augend/multiplicand is the third element
(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2)) (+ a1 a2)]
        [else (list a1 '+ a2)]))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))
(define (augend s) (caddr s))

(define (make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list m1 '* m2)]))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

; for testing
(display "2.58 a\n")
(deriv '(x + 2) 'x)

; b: apply "a" model to more general algebraic notation
; in those expressions, cddr is useful
; also because cddr uses a list data structure, we need procedure for a single value or symbol
; simplify is useful for that kind of case
(define (simplify exp)
  (if (null? (cdr exp)) (car exp) exp))

(define (augend s)
  (simplify (cddr s)))
(define (multiplicand p)
  (simplify (cddr p)))

; for testing
(display "2.58 b\n")
(deriv '(x + 3 * (x + y + 2)) 'x)