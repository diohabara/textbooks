;; 2.92
(define (install-polynomial-package)
  ;; internal procedures
  (define (make-poly variable-list term-list)
    (cons variable-list term-list))
  (define (variable-list p) (car p))
  (define (term-list p) (cdr p))
  (define (order-list term) (car term))
  (define (same-order? order-list1 order-list2)
    (equal? order-list1 order-list2))
  (define (=zero-term? L)
    (or (empty-termlist? L)
        (and (=zero? (coeff (first-term L)))
             (=zero-term? (rest-terms L)))))
  (define (=polynomial-zero? p)
    (=zero-term? (term-list p)))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order-list coeff) (list order-list coeff))
  (define (coeff term) (cadr term))
  
  (define (order-level order-list1 order-list2)
    (if (null? order-list1)
        0
        (let ((o1 (car order-list1)) (o2 (car order-list2)))
             (cond [(> o1 o2) 1]
                   [(< o1 o2) 2]
                   [else
                     (order-level (cdr order-list1) (cdr order-list2))]))))

  (define (add-terms L1 L2)
    (cond [(empty-termlist? L1) L2]
          [(empty-termlist? L2) L1]
          [else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
                 (let ((ol (order-level (order-list t1) (order-list t2))))
                      (cond [(= ol 1)
                             (adjoin-term
                               t1 (add-terms (rest-terms L1) L2))]
                            [(= ol 2)
                             (adjoin-term
                               t2 (add-terms L1 (rest-terms L2)))]
                            [else
                              (adjoin-term
                                (make-term (order-list t1)
                                           (add (coeff t1) (coeff t2)))
                                (add-terms (rest-terms L1)
                                           (rest-terms L2)))])))]))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
             (adjoin-term
               (make-term (mul-order-list (order-list t1) (order-list t2))
                          (mul (coeff t1) (coeff t2)))
               (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (mul-order-list order-list1 order-list2)
    (if (null? order-list1)
        '()
        (cons (add (car order-list1)
                   (car order-list2))
              (mul-order-list (cdr order-list1)
                              (cdr order-list2)))))

  (define (add-poly p1 p2)
    (make-poly (order-list p1)
               (add-terms (term-list p1)
                          (term-list p2))))

  (define (mul-poly p1 p2)
    (make-poly (order-list p1)
               (mul-terms (term-list p1)
                          (term-list p2))))

  (define (sub-poly p1 p2)
    (make-poly (order-list p1)
               (add-terms (term-list p1)
                          (negative-term (term-list p2)))))

  (define (negative-poly p)
    (make-poly (order-list p) (negative-term (term-list p))))

  (define (negative-term L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t (first-term L)))
             (adjoin-term
               (make-term (order-list t) (negative (coeff t)))
               (negative-term (rest-terms L))))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'negative '(polynomial)
       (lambda (p) (tag (negative-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put '=zero? '(polynomial) =polynomial-zero?)
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'debug '(polynomial) term-list)
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))