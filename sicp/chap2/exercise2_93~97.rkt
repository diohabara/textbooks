;; 2.93:modify the rational-arithmetic package to use generic operations
; but change make-rat so that it does not attempt to reduce fractions to lowest terms
(define p1 (make-polynomial 'x '((2 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational p2 p1))

(define (install-rational-package)
  ; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat p q)
    (cons p q))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (numer y) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (=rat-zero? x)
    (= (numer x) 0))
  (define (rat-equ? x y)
    (if (and (= (numer x) (numer y)) (= (denom x) (denom y)))
        #t
        #f))
  (define (negative-rat x)
    (make-rat (- (numer x) (denom x))))

  ;; interfac to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put '=zero? '(rational)
       (lambda (x) (=rat-zero> x)))
  (put 'make 'rational
       (lambda (p q) (tag (make-rat p q))))
  (put 'equ? '(rational rational)
       (lambda (x y) (rat-equ? x y)))
  (put 'negative '(rational)
       (lambda (x y) (tag (negative-rat x))))
  'done)

(define (make-rational p q)
  ((get 'make 'rational) p q))

;; 2.94
; Using div-terms, implement the procedure remainder-terms
; and use this to define gcd-terms as above.
; Now write a procedure gcd-poly that computes the polynomial GCD of two polys.
; Install in the system a geenric operation greatest-commo-divisor
; that reduces to gcd-poly for polynomials and to ordinary gcd for
; ordinary numbers
(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (reaminder-terms a b))))
(define (reaminder-terms a b)
  (cadr (div-terms a b)))
(define (gcd-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (gcd-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- GCD-POLY" (list p1 p2))))


;; 2.95
; just use 2.94 procedure

;; 2.96
; a:implement procedure pseudoreaminder-terms and modify gcd-terms
; pseudoreaminder-terms:multiply p by coefficient of q and divide it by q
(define (pseudoremainder-terms a b)
  (let ((o1 (order (first-term a)))
        (o2 (order (first-term b)))
        (c (coeff (first-term b))))
    (let (divident (mul-terms (make-term 0
                                         (expt c (+ 1 (- o1 o2))))
                              a))
      (cadr (div-tems divident b)))))
(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (pseudoreaminder-terms a b))))

; b:divide all the coefficinets by their int gcd
(define (gcd-terms a b)
  (if (empty-termlist? b)
      (let ((coeff-list (map cadr a)))
        (let (gcd-coeff (apply gcd coeff-list))
          (div-terms a (make-term 0 gcd-coeff))))
      (gcd-terms b (pseudoremainder-terms a b))))

;; 2.97
; a: implement reduce-terms that takes two lists n and d as arguments
; and returns a list nn, dd, which are n and d reduced to lowest terms
; plus, reduce-poly
(define (integerizing-factor o1 t2)
    (let ((o2 (order t2))
          (c (coeff t2)))
(expt c (+ 1 (- o1 o2)))))
(define (reduce-terms n d)
  (let ((g (gcd-terms n d)))
    (let ((c (coeff (first-term g)))
          (o1 (if (> (order (first-term n)) (order (first-term d)))
                  (order (first-term n))
                  (order (first-term d))))
          (o2 (order (first-term g))))
      (let ((integerizing-factor (exp c (- o1 o2 1))))
        (list (car (div-terms (mul-term-by-all-terms
                               (make-term 0 integerizing-factor) n)
                              g))
              (car (div-terms (mul-term-by-all-terms
                               (make-term 0 integerizing-factor) d)
                              g)))))))
(define (reduce-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (let ((reduced-terms (reduce-terms (term-list p1)
                                         (term-list p2))))
        (list (make-poly (variable p1) (car reduced-terms))
              (make-poly (variable p1) (cadr reduced-terms))))
      (error "Polys not in same var: REDUCE-POLY")))


; b: define a procedure analogous to reduce-terms that does
; what the original make-rat did for integers
(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))
(define (reduce a b)
  (apply-generic 'reduce a b))

;; scheme-number
(define (reduce-integers n d)
  (let ((g (gcd n d))) (list (/ n g) (/ d g))))
(put 'reduce '(scheme-number scheme-number)
     (lambda (x y) (tag (reduce-integers x y))))