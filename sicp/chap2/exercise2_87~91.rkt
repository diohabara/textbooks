;; 2.87
; for polynomial package
(define (=zero-term? term-list)
  (or (empty-termlist? L)
      (and (=zero? (coeff (first-term term-list)))
           (=zero-term? (rest-terms term-list)))))
(define (=polynomial-zero? p)
  (=zero-term? (term-list p)))
(put '=zero? '(polynomial) =polymomial-zero?)

; 2.88:extend polynomial system to include subtraction of polynomials
(define (negative x) (apply-generic 'negarive x))
; for integer package
(put 'negative '(integer)
     (lambda (x) (tag (- x))))
; for ratoinal pakage
(define (negative-rat x)
  (make-rat (- (numer x)) (denom x)))
(put 'negative '(rational)
     (lambda (x) (tag (negative-rat x))))
; for real package
(put 'negative '(real)
     (lambda (x) (tag (- x))))
; for complex package
(put 'negative '(complex)
     (lambda (z) (tag (make-from-real-imag (negative (real-part z))
                                           (negative (imag-part z))))))
; for polynomial package
(define (negative-poly p)
  (make-poly (variable p) (negative-term (term-list p))))
(define (negative-term term-list)
  (if (empty-termlist? term-list)
      (the-empty-termlist)
      (let ((t (first-term term-list)))
        (adjoin-term
         (make-term (order t) (negative (coeff t)))
         (negative-term (rest-term term-list))))))
(put 'negative '(polynomial)
     (lambda (p) (tag (negative-poly p))))
; for polynomial package (sub)
(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (negative-term (term-list p2))))
      (error "Polys not in same var -- ADD-POLY" (list p1 p2))))
(put 'sub '(polynomial polynomial)
     (lambda (p1 p2) (tag (sub-poly p1 p2))))

;; 2.89:define procedures for dense polynomial
(define (first-term term-list)
  (make-term (- (len term-list) 1) (car term-list)))
(define (adjoijn-term term term-list)
  (cond [(=zero? term) term-list]
        [(=equ? (order term) (length term-list))
         (cons (coeff term) term-list)]
        [else (adjoin-term term (cons 0 term-list))]))

;; 2.90:make a new package for both dense and sparse polynomial
; sparse
(define (install-sparse-term-package)
  ; internal procedures
  (define (make-sparse-term order coeff)(list order coeff))
  (define (the-empty-sparse-termlist) '())
  (define (empty-sparse-termlist? term-list) (null? term-list))
  (define (first-sparse-term term-list) (car term-list))
  (define (rest-sparse-terms temr-list) (cdr term-list))
  (define (order-sparse term) (car term))
  (define (coeff-sparse term) (cadr term))
  (define (adjoin-sparse-term term term-list)
    (if (=zero? (coeff-sparse term))
        term-list
        (cons term term-list)))
  (define (=zero-sparse-term? term-list)
    (or (empty-sparse-termlist? term-list)
        (and (=zero? (coeff-sparse (first-sparse-term term-list)))
             (=zero-sparse-term? (rest-sparse-terms term-list)))))
  (define (add-sparse-terms list1 list2)
    (cond [(=empty-sparse-termlist list1) list2]
          [(=empty-sparse-termlist list2) list1]
          [else
           (let ((t1 (first-sparse-term list1)) (t2 (first-sparse-term list2)))
             (cond [(> (order-sparse t1) (order-sparse t2))
                    (adjoin-sparse-term
                     t1
                     (add-sparse-terms (rest-sparse-terms list1) list2))]
                   [(< (order-sparse t1) (order-sparse t2))
                    (adjoin-sparse-term
                     t2
                     (add-sparse-terms list1 (rest-sparse-terms list2)))]
                   [else
                    (adjoin-sparse-term
                     (make-sparse-term (order-sparse t1)
                                       (add (coeff-sparse t1) (coeff-sparse t2)))
                     (add-sparse-terms (rest-sparse-terms list1)
                                       (rest-sparse-terms list2)))]))]))
  (define (mul-sparse-terms list1 list2)
    (if (empty-sparse-termlist? list1)
        (the-empty-sparse-termlist)
        (add-sparse-terms (mul-sparse-term-by-all-sparse-terms
                           (first-sparse-term list1) list2)
                          (mul-sparse-terms (rest-sparse-terms list1) list2))))
  (define (mul-sparse-term-by-all-sparse-terms t1 L)
    (if (empty-sparse-termlist? L)
        (the-empty-sparse-termlist)
        (let ((t2 (first-sparse-term L)))
          (adjoin-sparse-term
           (make-sparse-term (+ (order-sparse t1) (order-sparse t2))
                             (mul (coeff-sparse t1) (coeff-sparse t2)))
           (mul-sparse-term-by-all-sparse-terms t1 (rest-sparse-terms L))))))
  (define (negate-sparse-term L)
    (if (empty-sparse-termlist? L)
        (the-empty-sparse-termlist)
        (let ((t (first-sparse-term L)))
          (adjoin-sparse-term
           (make-sparse-term (order-sparse t) (negate (coeff-sparse t)))
           (negate-sparse-term (rest-sparse-terms L))))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'sparse-term x))
  (put '=zero-term? '(sparse-term) =zero-sparse-term?)
  (put 'order '(sparse-term) order-sparse)
  (put 'add-terms '(sparse-term sparse-term)
       (lambda (x y) (tag (add-sparse-terms x y))))
  (put 'mul-terms '(sparse-term sparse-term)
       (lambda (x y) (tag (mul-sparse-terms x y))))
  (put 'negate-term '(sparse-term)
       (lambda (x) (tag (negate-sparse-term x))))
  (put 'make-from-sparse 'sparse-term
       (lambda (sparse-term-list) (tag sparse-term-list)))
  (put 'make-from-dense 'sprse-term
       (labmda (dense-term-list) (tag (dense->spase dense-term-list))))
  'done)
(define (make-sparse-term term-list)
  ((get 'make-from-sparse 'sparse-term) term-list))

; dense
(define (install-dense-term-package)
  ;; internal procedures
  (define (adjoin-dense-term term term-list)
    (cons term term-list))
  (define (the-empty-dense-termlist) '())
  (define (empty-dense-term term-list) (car term-list))
  (define (first-dense-term term-list) (cdr term-list))
  (define (rest-dense-terms term-list) (length (rest-dense-terms term-list)))
  (define (order-dense-term term-list) (first-dense-term term-list))
  (define (=zero-dense-term? L)
    (or (empty-dense-termlist? L)
        (and (=zero? (coeff-dense-term L))
             (=zero-dense-term? (rest-dense-terms L)))))
  (define (normalize-dense-term L)
    (cond [(empty-dense-termlist? L) L]
          [(=zero? (first-dense-term L))
           (normalize-dense-term (rest-dense-terms L))]
          [else L]))
  (define (add-dense-terms L1 L2)
    (define (add-rterms R1 R2)
      (cond [(empty-dense-termlist? R1) R2]
            [(empty-dense-termlist? R2) R1]
            [else
             (adjoin-dense-term (add (first-dense-term R1)
                                     (first-dense-term R2))
                                (add-rterms (cdr R1) (cdr R2)))]))
    (cond [(empty-dense-termlist? L1) L2]
          [(empty-dense-termlist? L2) L1]
          [else
           (normalize-dense-term (reverse (add-rterms (reverse L1)
                                                      (reverse L2))))]))
  (define (expand-dense-term L n)
    (if (= n 0)
        L
        (expand-dense-term
         (adjoin-dense-term (make-integer 0) L) (- n 1))))
  (define (mul-dense-terms L1 L2)
    (define (mul-dense-terms-sub n L1 L2)
      (if (= n 0)
          (mul-dense-term-by-all-dense-terms 0 (first-dense-term L1) L2)
          (add-dense-terms
           (mul-dense-term-by-all-dense-terms n (first-dense-term L1) L2)
           (mul-dense-terms-sub (- n 1) (rest-dense-terms L1) L2))))
    (if (or (empty-dense-termlist? L1) (empty-dense-termlist L2))
        (the-empty-dense-termlist)
        (mul-dense-terms-sub (order-dense-term L1) L1 L2)))
  (define (mul-dense-term-by-all-dense-terms n t1 L)
    (reverse (expand-dense-term (map (lambda (t) (mul t1 t)) (reverse L)) n)))
  (define (negate-dense-term) (map negate L))

  ; interface to rest of the system
  (define (tag x) (attach-tag 'dense-term x))
  (put '=zero-term? '(dense-term) =zero-dense-term?)
  (put 'add-terms '(dense-term dense-term)
       (lambda (x y) (tag (add-dense-terms x y))))
  (put 'mul-terms '(dense-term dense-term)
       (lambda (x y) (tag (mul-dense-terms x y))))
  (put 'negate-term '(dense-term)
       (lambda (x) (tag (negate-dense-term x))))
  (put 'make-from-sparse 'dense-term
       (lambda (sparse-term-list) (tag (sparse->dense sparse-term-list))))
  (put 'make-from-dense 'dense-term
       (lambda (dense-term-list) (tag dense-term-list)))
  'done)

(define (make-dense-term term-list)
  ((get 'make-from-dense 'dense-term) term0list))

; interconversion between sparse term-list and dense term-list
(define (dense->sparse term-list)
  (define (iter result i term-list)
    (if (null? term-list)
        result
        (iter (cons (list i (car term-list)) result ) (+ i 1) (cdr term-list))))
  (iter '() 0 (reverse term-list)))
(define (sprase->dense i term-list)
  (define (iter result i term-list)
    (if (null? term-list)
        result
        (let ((term (car term-list)))
          (let ((j (car term)))
            (if (= i j)
                (iter (cons (cadr term) result) (+ i 1) (cdr term-list))
                (iter (cons (make-integer 0) result) (+ i 1) term-list))))))
  (iter '() 0 (reverse term-list)))
(put-coercion 'dense-term 'sparse-term
              (lambda (d) (make-sparse-term (dense->sprase (contents d)))))
(put-coercion 'sparse-term 'dense-term
              (lambda (s) (make-dense-term (sprase->dense (contents s)))))
(put-coercion 'dense-term 'dense-term identity)
(put-coercion 'sprase-term 'sprase-term identity)

; polynomial
(defien (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (=zero-polu? p)
    (=zero-term? (term-list p)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY" (list p1 p2))))
  (define (negate-poly p)
    (make-poly (variable p) (negate-term (term-list p))))
  (define (sub-poly p1 p2)
    (add-poly p1 (negete-poly p2)))

  ; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial)
       =zero-poly?)
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynimial var terms)
  ((get 'make 'polynomial) var terms))

; generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car tyep-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond [t1->t2
                         (apply-generic op (t1->t2 a1) a2)]
                        [t2->t1
                         (apply-generic op a1 (t2->t1))]
                        [else (error "No method for these types" (list op type-tags))])))
              (error "No method for these types" (list op type-tags)))))))
(define (add-terms x y) (apply-generic 'add-terms x y))
(define (mul-terms x y) (apply-generic 'mul-terms x y))

;; 2.91:fill in div-terms's missing expressions
; plus, define div-term that returns a list of the quotient and remainder polys
(define (div-terms L1 L2)
  (if (empty-termslist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      ; get the highest order
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        ; stop when divisor's order is higher that dividend's one
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2))) ; get coeff
                  (new-o (- (order t1) (order t2)))) ; get order
              (let ((rest-of-result
                        (div-terms
                         (sub-terms L1 (mul-terms (list (make-term new-o new-c)) L2))
                         L2)
                        ))
                (list (add-terms (list (make-term new-o new-c))
                                 (car rest-of-result))
                      (cadr rest-of-result))))))))
(define (div-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                       (div (term-list p1)
                            (term-list p2)))
            (error "Polys not in same var -- DIV-POLY" (list p1 p2))))