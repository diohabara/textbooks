;; 2.73
; program in section 2.3.2 that perform symbolic differentiation
; same variables: same symbols
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; make-sum/product: construct as list
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

; sum: a list whose first element is +
(define (sum? x) (and (pair? x) (eq? (car x) '+)))

; addend: the second item of the sum list 
(define (addend s) (cadr s))

; augend: the third item of the sum list
(define (augend s) (caddr s))

; product: a list whose first element is *
(define (product? x) (and (pair? x) (eq? (car x) '*)))

; multiplier: the second item of the product list
(define (multiplier p) (cadr p))

; multiplicand: the third item of the product list
(define (multiplicand p) (caddr p))

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


; transform this program into data-directed style
(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp) (if (same-variable? exp var) 1 0)]
        [else ((get 'deriv (operator exp))
               (operands exp) var)]))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; a: 1, explain what was done. 2, why can't we assimilate the predicates into data-directed dispatch
; 1, look up expression in the table indexed by operators and operands
; 2, number? and variable? do not have list and type

; b: write the procedures for derivatives of sums nad products
; plus, auxiliary code required to isntall them in the table by the program
(define (install-sum-package)
  ;; interface procedures
  (define (addend s)
    (cadr s))
  (define (augend s)
    (caddr s))
  (define (make-sum a1 a2)
    (cons a1 a2))
  (define (deriv-sum s)
    (make-sum (deriv (addend s)) (deriv (augend s))))

  ;; interface  to the rest of the system
  (define (tag x) (attach-tag '+ x))
  (put 'deriv '(+) deriv-sum)
  (put 'make-sum '+
       (lambda (x y) (tag (make-sum x y))))
  'done)


(define (install-product-package)
  ;; interface procedures
  (define (multiplier p)
    (cadr p))
  (define (multiplicand p)
    (caddr p))
  (define (make-product a1 a2)
    (cons a1 a2))
  (define (deriv-product p)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplier exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))

  ;; interface  to the rest of the system
  (define (tag x) (attach-tag '* x))
  (put 'deriv '(*) deriv-product)
  (put 'make-product '*
       (lambda (x y) (tag (make-product x y))))
  'done)

;; c: install exponent differentiation in data-directed style
(define (install-exponent-package)
  ;; interface procedures
  (define (base s) (cadr s))
  (define (exponent s) (caddr s))
  (define (make-exponentiation base e)
    (cond [(=number? e 0) 1]
          [(=number? e 1) base]
          [else (list `** base e)]))
  (define (deriv-exponentiation exp var)
    (let ((make-product (get 'make '*)))
      (make-product
       (make-product
        (exponent exp)
        (make-exponentiation (base exp (- (exponent exp) 1))))
       (deriv (base exp) var))))
    
  ;; interfase to the rest of the system
  (define (tag x) (attach-tag '** x))
  (put 'make-exponentiation '** make-exponentiation)
  (put 'deriv '** deriv-exponentiation)
  'done)

;; d: just exchange the places of type and operator

;; 2.74
; in this section, I do not make constructors in a data-directed way.
;; a: implement get-record procedure
(define (get-record employee-name file)
  ((get 'get-record (division file)) employee-name))
;; b: implement get-salary procedure
(define (get-salary division record)
  ((get division salary) record))
;; c: impleent find-employee-record
(define (find-employee-record employee-name file-list)
  (if (null? file-list)
      #f
      (append (get-record (car file-list))
              (find-emplyee-record name (cdr file-list)))))
;; d: add the name of the company as new division, and implement as above