;;; Building Abstractions with Data
;; 2.1 Introduction to Data Abstraction
;; 2.1.1 Example: Arigthmatic Operations for Rational Nubmers
; numer, denom and make-rat haven't yet been undefined
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
; Pairs
; cons: construct
; car: Contents of Address part of Register
; cdr: Contents of Decrement part of Register, pronounced could-er
; implement undefined procedures mentioned above
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; improve make-rat using gcd
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; 2.1.2 Abstraction Barriers
; alternate version of constructor and selectro procedures
; reduce rational numers to the lowest terms in selecting
(define (make-rat n d) (cons n d))
(define (numer x)
  (let ((g(gcd (car x) (cdr x))))
    (/ (car x) g)))
(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

;; 2.1.3 Waht is Meant by Data?
; cons, car, cdr
#|(define (cons x y)
  (define (dispatch m)
    (cond [(= m 0) x]
          [(= m 1) y]
          [else (error "Argument not 0 or 1: CONS" m)]))
  dispatch)
(define (car z) (z 0))
(define (cdr z) (z 1))|#

;; 2.1.4 Extended Exercise: Internval Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

;; 2.2 Hierarchical Data and the Closure Property
;; 2.2.1 Representing Sequencese 
(define one-through-four (list 1 2 3 4))
; return n-th element
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))

; retrun the number of items in a list(recursive)
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))

; (iterative)
(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

; make a new list, combining two lists
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; scale number in a list by factor
(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor))))

; implementation of mapping
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

; scaling using mapping
(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

;; 2.2.2 Hierarchical Structures
(define (count-leaves x)
  (cond [(null? x) 0]
        [(not (pair? x)) 1]
        [else (+ (count-leaves (car x))
                 (count-leaves (cdr x)))]))

; mapping over trees(recursive)
(define (scale-tree tree factor)
  (cond [(null? tree) nil]
        [(not (pair? tree)) (* tree factor)]
        [else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor))]))

; mapping over trees(regard trees as sequence of sub-trees)
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;; 2.2.3 Sequences as Conventional Interfaces
; sum of thel squares of the leaves that are odd
(define (sum-odd-squares tree)
  (cond [(null? tree) 0]
        [(not (pair? tree))
         (if (odd? tree) (square tree) 0)]
        [else  (+ (sum-odd-squares (car tree))
                  (sum-odd-squares (cdr tree)))]))

; even Fibonacci numbers
(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fib (- n 1))
                 (fib (- n 2)))]))
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        hil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

; filter sequence
(define (filter predicate sequence)
  (cond [(null? sequence) nil]
        [(predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence)))]
        [else (filter predicate (cdr sequence))]))

; accumulate sequence
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; generage sequence of integers
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

; enumerate the leaves of a tree
(define (enumerate-tree tree)
  (cond [(null? tree) nil]
        [(not (pair? tree)) (list tree)]
        [else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree)))]))

; reformulation of sum-odd-squares
(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

;; reformulation of even Fibonacci numbers
(define (even-fibs n)
  (accumulate
   cons
   nil
   (filter even? (map fib (enumerate-interval 0 n)))))

; construct a list of the squares of the first n + 1 Fibonacci numbers
(define (square x) (* x x))
(define (list-fib-squares n)
  (accumulate
   cons
   nil
   (map square (map fib (enumerate-interval 0 n)))))

; product of squares of odd elements in a sequence
(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

; return the highest salary of programmer
(define (salary-of-highest-paid-programmer records)
  (accumulate max 0 (map salary (filter programmer? records))))

;; Nested Mappings
; combination of mapping and accumulating with append
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; predicate of prime number
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

; make-pair of two integers and the sum of them
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; combine sequence 
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))

; permutations of S
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
(define (permutations s)
  (if (null? s) ; empty set?
      (list nil) ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

;; 2.2.4 Example: A Picture Language
; I skipped this section

;;; 2.3 SYmbolic Data
;; 2.3.1 Quotation
; check if the item is contained in the list
(define (memq item x)
  (cond [(null? x) false]
        [(eq? item (car x)) x]
        [else (memq item (cdr x))]))

;; 2.3.2 Example: Symbolic Differentiation
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
; variable: symbol
(define (variable? x) (symbol? x))

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

; better make-sum: simplify the result
(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2))
         (+ a1 a2)]
        [else (list '+ a1 a2)]))

; =number?: check an expression is equal to a given number
(define (=number? exp num) (and (number? exp) (= exp num)))

; better make-product: simplify the result
(define (make-product m1 m2)
  (cond [(or (=number? m1  0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list '* m1 m2)]))

;; 2.3.3 Example: Representing Sets
; Sets as unordered lists
(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(equal? x (car set)) #t]
        [else (element-of-set? x (cdr set))]))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2)) '()]
        [(element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2))]
        [else (intersection-set (cdr set1) set2)]))
; Sets as ordered lists
(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(= x (car set)) #t]
        [(< x (car set)) #f]
        [else (element-of-set? x (cdr set))]))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond [(= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2)))]
              [(< x1 x2)
               (intersection-set (cdr set1) set2)]
              [(< x2 x1)
               (intersection-set set1 (cdr set2))]))))

; Sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(= x (entry set)) #t]
        [(< x (entry set))
         (element-of-set? x (left-branch set))]
        [(> x (entry set))
         (element-of-set? x (right-branch set))]))

(define (adjoin-set x set)
  (cond [(null? set) (make-tree x '() '())]
        [(= x (entry set)) set]
        [(< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set))]
        [(> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set)))]))

; Sets and infomation retrieval
(define (lookup given-key set-of-records)
  (cond [(null? set-of-records) #f]
        [(equal? given-key (key (car set-of-records)))
         (car ser-of-records)]
        [else (lookup given-key (cdr set-of-records))]))

;; 2.3.4 Example: Huffman Encoding Trees
; Generating Huffman trees
; Representing Huffman trees
; constructor and selector of leaf
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; constructor of tree
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; selector of tree
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; The decoding procedure
; bits: a list of zeros and ones
; tree: Huffman tree
; next-branch: current-branch(tree)'s left or right branch
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond [(= bit 0) (left-branch branch)]
        [(= bit 1) (right-branch branch)]
        [else (error "bad bit: CHOOSE-BRANCH" bit)]))

; Sets of weighted elements
; adjoin-set for Huffman trees
(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(< (weight x) (weight (car set))) (cons x set)]
        [else (cons (car set)
                    (adjoin-set x (cdr set)))]))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

;; 2.4 Multiple Representations for Abstract Data
;; 2.4.1 Representaions for Complex Numbers
; constructor and selector
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (real-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (real-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mg-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

; Ben's rectangular form
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (img-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

; Alyssa's polar form
(define (real-part z) (* (magnitude z) (cos (angle z))))
(define (imag-part z) (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-mag-ang r a) (cons r a))

;; 2.4.2 Tagged data
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

; distinguish tag
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z) (eq? (type-tag z) 'polar))

; revised representation
; Ben
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (ange-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

; Alyssa
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

; extract data from tagged lists
(define (real-part z)
  (cond [(rectangular? z)
         (real-part-rectangular (contents z))]
        [(polar? z)
         (real-part-polar (contents z))]
        [else (error "Unknown type: REAL-PART" z)]))
(define (imag-part z)
  (cond [(rectangular? z)
         (iamg-part-rectangular (contents z))]
        [(polar? z)
         (imag-part-polar (contents z))]
        [else (error "Unknown type: IMAG-PART" z)]))
(define (magnitude z)
  (cond [(rectangular? z)
         (magnitude-rectangular (contents z))]
        [(polar? z)
         (magnitude-polar (contents z))]
        [else (error "Unknown type: MAGNITUDE" z)]))
(define (angle z)
  (cond [(rectangular? z)
         (angle-rectangular (contents z))]
         [(polar? z)
          (angle-polar (contents z))]
         [else (error "Unknown type: ANGLE" z)]))

; (x, y) -> rectangular form
; (r, a) -> polar form
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; 2.4.3 Data-Directed Programming and Additivity
; (put op type item): install the item in the table, indexed by the op and type
; (get op type): look up the op, type entry in the table and return the item
; rectangle version
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (defien (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (iamg-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'iamg-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-iamg 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; polar version
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-ima x y))))
  (put 'make-form-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; apply a procedure to a arguments
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-rags)))
      (if proc
           (apply proc (map contents args))
           (errror
            "No method for these types: APPLY-GENERIC"
            (list op type-tags))))))

; definition of generic selectors using apply-generic
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

; Message passing
(define (make-from-real-imag  x y)
  (define (dispatch op)
    (cond [(eq? op 'real-part) x]
          [(eq? op 'imag-part) y]
          [(eq? op 'magnitude) (sqrt (+ (square x) (sqaure y)))]
          [(eq? op 'angle) (atan y x)]
          [else (error "Unknown op: MAKE-FROM-REAL-IMAG" op)]))
  dispatch)
(define (apply-generic op arg) (arg op))

;; 2.5 Systems with Generic Operations
;; Generic Arithmetic Operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; package that performs rational arithmetic
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (add-sub x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rationa n d)
  ((get 'make 'rational) n d))

; package for complex number
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; 2.5.2 Combining Data of Different Types
; to be included in the complex package
(define (add-complex-to-schemenum z x)
  (make-from-real-imag (+ (real-part z) x) (imag-part z)))
(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))

; coercion
(define (schem-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number
              'complex
              scheme-number->complex)

; coerce a type to the other type. if it's impossible do otherwise
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond [t1->t2
                         (apply-generic op (t1->t2 a1) a2)]
                        [t2->t1
                         (apply-generic op a1 (t2->t1 a2))]
                        [else (error "No method for these types"
                                     (list op (type-tags)))])))
              (error "NO method for these types"
                     (list op type-tags)))))))

;; 2.5.3 Example:Symbolic Algebra
;; Arithmetic on polynomials
(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: ADD-POLY" (list p1 p2))))
(define (mul-polhy p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poy (variable p1)
                (mul-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: MUL-POLY" (list p1 p2))))
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of body
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: ADD-POLY" (list p1 p2)))
  (define (mul-polhy p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poy (variable p1)
                  (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-only p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)


; the-empty-termlist:return the empty list
; adjoin-term: adjoin a new term to a term list
; first-term:extract the highest-order tem from a term list
; rest-terms:retrun all but the highest-order term
; add-terms:construct the term list from the sum of two polynomials
(define (add-terms L1 L2)
  (cond [(empty-termlist? L1) L2]
        [(empty-termlist? L2) L1]
        [else
         (let ((t1 (first-term L1))
               (t2 (first-term L2)))
           (cond [(> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2))]
                 [(< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2)))]
                 [else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))]))]))

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
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t2) (rest-terms L)))))

;; Representing term lists
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-term term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var temrs))

;; Hierarchies of types in symbolic algebra