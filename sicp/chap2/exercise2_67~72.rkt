;; codes from a section
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

 (define (element-of-set? x set) 
   (cond ((null? set) false) 
         ((equal? x (car set)) true) 
         (else (element-of-set? x (cdr set))))) 

;; 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; for testing 
(display "2.67\n")
(decode sample-message sample-tree) ; ADABBACA

;; 2.68
; return the list of bits that gives the encoded message
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
; define encode-symgol, which returns the list of bits that encodes a given symbol
(define (encode-symbol symbol tree)
  (define (have-symbol? branch)
    (if (leaf? branch)
        (equal? symbol (symbol-leaf branch))
        (element-of-set? symbol (symbols branch))))
  (let ((left (left-branch tree))
        (right (right-branch tree)))
    (cond [(have-symbol? left)
           (if (leaf? left)
               '(0)
               (cons 0 (encode-symbol symbol left)))]
          [(have-symbol? right)
           (if (leaf? right)
               '(1)
               (cons 1 (encode-symbol symbol right)))]
          [else (error "bad symbol -- ENCODE-SYMBOL" symbol)])))

; for testing
(display "2.68\n")
(encode (decode sample-message sample-tree) sample-tree)

;; 2.69
; paris: a list of symbol-frequency pairs
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; make-leaf-set: transform the list of pairs into an ordered set of leaves
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))
; successive-merge merge the smallest-weight elements of the set using make-code-tree
; leaf-set is a list which contains only a leaf-set, return it
; otherwise, first: the first leaf-set, seconde: the second leaf-set, rest: the remaining leaf-sets
(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (let ((first (car leaf-set))
            (second (cadr leaf-set))
            (rest (cddr leaf-set)))
        (successive-merge (adjoin-set (make-code-tree first second)
                                    rest)))))

;; 2.70
; encode and
(display "2.70\n")
(define rock-tree (generate-huffman-tree '((A 2) (NA 16) (BOOM  1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))
(define rock-song  '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
(define encode-rock-song (encode rock-song rock-tree))
encode-rock-song

;; 2.71
; suppose I have an alphabet of n symbols, and the relative frequencies of the symbols
; are 1, 2, 4, 2^(n-1)
; skip the sketch
; the minimum bits required is 2^n - 1
; because 1+2+4+...+2^(n-1) is the sum of geometric progression, whose
; initial value is 1, common ratio is 2 and the number of terms is n

;; 2.72
; the order of procedure in exrcise2.68 is O(n^2)
; below, only trees of exercise 2.71 is considered,
; when trying to encode the most frequent symbol, or leaf
; O(n)
; whent trying to encode the least frequent symbol, or root
; O(n^2)