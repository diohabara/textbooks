; 2.21
; without using higher-order procedures
(define (square-tree tree)
  (cond [(null? tree) nil]
        [(not (pair? tree)) (* tree tree)]
        [else (cons (square-tree (car tree))
                    (square-tree (cdr tree)))]))

; for testing
(display "2.21\n1\n")
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

; by using map and recursion
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

; for testing
(display "2\n")
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;; 2.31
; abstraction of procedure above
(define (tree-map proc tree)
  (cond [(null? tree) nil]
        [(not (pair? tree)) (proc tree)]
        [else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree)))]))
(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))

; for testing
(display "2.32\n")
(define (square-tree tree) (tree-map square tree))
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;; 2.32
; generate the set of subset of a set
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
        rest)))))

; for testing
(display "2.23\n")
(subsets (list 1 2 3))