;; 2.81
; Louis Reasoner's implementation
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number
              'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;; a: what happens, if the aruguments are two scheme-number or two complex
; apply-generic calls itself recursively on coerced types, so it goes into
; infinite loop

;; b: Louis is not correct

;; c: modify apply-generic
(define (apply-generic op . args)
  (define (no-method type-tags)
    (erorr "No method for these types"
           (list type-tags)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (no-method type-tags)
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond [t1->t2
                             (apply-generic op (t1->t2 a1) a2)]
                            [t2->t1
                             (apply-generic op a1 (t2->t1 a2))]
                            [else (no-method type-tags)]))))
              (no-method type-tags))))))

;; 2.82: generalize apply-generic for multiple arguments
(define (apply-generic op . args)
  (define (coerce-all args target-type-tag) ; transform all elements' into target-type-tag
    (if (null? args)
        '()
        (let ((proc (get-coercion (type-tag (car args)) target-type-tag)))
              (if proc
                  (cons (proc (car args)) (coerce-all (cdr args) target-type-tag))
                  (cons (car args) (coerce-all (cdr args) target-type-tag))))))
  (let ((type-tags (map type-tag args))) ; list of types of arguments
    (define (coercion-all-first-type-tag types)
      (let ((first-type (car types)))
            (if (null? first-type)
                #f
                (let ((first-type-args (coerce-all args first-type)))
                  (let ((proc (get op (map type-tag first-type-args))))
                    (if proc
                        (apply proc (map contents first-type-args))
                        (coercion-all-first-type-tag (cdr types))))))))
    (coercion-all-first-type-tag type-tags)))

;; 2.83: implement raise except that for complex
(define (raise x) (apply-generic 'rase x))
; for scheme-number
(put 'raise '(scheme-number)
     (lambda (x) (make-rational x 1)))
; for rational-number
(define (raise-rat x)
  (make-real (/ (* (number x) 1.0) (denom x))))
(put 'raise '(rational-number)
     (lambda (x) (raise-rat x)))
; for real-number
(put 'raise '(real)
     (lambda (x) (make-from-real-imag x 0)))

;; 2.84: modify apply-generic using raise operaton of ex2.83

(define (higher-type x y)
  (let ((tower '(complex real rational scheme-number)))
    (define (iter twr)
      (if (null? twr)
          #f
          (cond [(eq? x (car twr)) x]
                [(eq? y (car twr)) y]
                [else (iter (cdr twr))])))
    (iter tower)))
(define (coerce-higher-type items)
  (let ((item1 (car items))
        (item2 (cadr items)))
    (let ((type1 (type-tag item1))
          (type2 (type-tag item2)))
      (if (eq? type1 type2)
          items
          (let ((tag (higher-type type1 type2)))
            (if (eq? tag type1)
                (coerce-higher-type (list item1 (raise item2)))
                (coerce-higher-type (list (raise item1) item2))))))))
(define (apply-generic op . args)
  (define (no-method type-tags)
    (erorr "No method for these types"
           (list type-tags)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc 
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags)))
                (if (eq? type1 type2)
                    (no-method type-tags)
                    (let ((coerced-args (coerce-higher-type args)))
                      (let ((proc (get op (map type-tag coerced-args)))
                            (if proc
                                (apply proc (map contents coerced-args))
                                (no-method type-tags)))))))
              (no-method type-tags))))))

;; 2.85: define drop that lower the type of object
; project that pushes an object down in the tower
(define (install-project-package)
  (define (complex->real x)
    (make-real (real-part x)))
  (define (real->rational x)
    (make-rational (x->integer x) 1))
  (define (rational->integer x)
    (let ((n (car x))
          (d (cdr x)))
      (make-integer (round (/ n d)))))
  (put 'project 'complex complex->real)
  (put 'project 'real real->rationla)
  (put 'project 'rational rational->integer)
  'done)

(install-project-package)

(define (project x)
  (let ((proc (get 'project (type-tag x))))
    (if proc
        (proc (contents x))
        #f)))

(define (drop x)
  (if (pair? x)
      (let ((projected (project x)))
        (if projected
            (if (equ? (raise projected) x)
                (drop projected)
                x)
            x))
      x))

(define (apply-generic op . args)
  (define (no-method type-tags)
    (erorr "No method for these types"
           (list type-tags)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags))
          (if proc
              (drop (apply proc (map contents args)))
              (if (= (length args) 2)
                  (let ((type1 (car type-tags))
                        (type2 (cadr type-tags)))
                    (if (eq? type1 type2)
                        (no-method type-tag)
                        (let ((coerced-args (coerce-higher-type args))
                              (let ((proc (get op (map type-tag coerced-args)))
                                    (if proc
                                        (drop (apply proc (map contents coerced-args)))
                                        (no-method type-tags))))))))
                  (no-method type-tags)))))))
                    
                        
                        