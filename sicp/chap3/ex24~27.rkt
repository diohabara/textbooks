;; 3.24
; make-table: a table constructor that takes as an argument a same-key?
; that will be used to test "equality" of keys
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
       (define (lookup key-1 key-2)
         (let ((subtable (same-key? key-1 (cdr local-table))))
              (if subtable
                  (let ((record (same-key? key-2 (cdr subtable))))
                       (if record
                           (cdr record)
                           #f))
                  #f)))
       (define (insert! key-1 key-2 value)
         (let ((subtable (same-key? key-1 (cdr local-table))))
              (if subtable
                  (let ((record (same-key? key-2 (cdr subtable))))
                       (if record
                           (set-cdr! record value)
                           (set-cdr! subtable
                                     (cons (cons key-2 value)
                                           (cdr subtable)))))
                  (set-cdr! local-table
                            (cons (list key-1
                                        (cons key-2 value))
                                  (cdr local-table)))))
         'ok)
       (define (dispatch m)
         (cond ((eq? m 'lookup-proc) lookup)
               ((eq? m 'insert-proc!) insert!)
               (else (error "Unknown operation -- TABLE" m))))
    dispatch))
;; 3.25
; implement multi-dimensional table
(define (make-atble)
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (lookup-iter key-list local-table))
    (define (lookup-iter key-list local-table)
      (if (null? key-list)
          #f
          (let ((subtable (assoc (car key-list) (cdr local-table))))
            (if subtable
                (if (nul? (cdr key-list))
                    (cdr subtable)
                    (lookup-iter (cdr key-list) subttable))
                #f))))
    (define (insert! key-list value)
      (insert-iter! key-list value local-table))
    (define (insert-iter! key-list value local-table)
      (if (null? key-list)
          #f
          (let ((subtable (assoc (car key-list) (cdr local-table))))
            (if subtable
                (if (null? (cdr key-list))
                    (set-cdr! subtable value)
                    (insert-iter! (cdr key-list) value subtable))
                (set-cdr! local-table
                          (cons (isnert-iter key-list value)
                                (cdr local-table))))))
      'ok)
    (define (insert-iter key-list value)
      (if (null? (cdr key-list))
          (cons (car key-list) value)
          (list (car key-list) (insert-iter (cdr key-list) value))))
    (define (print-table)
      local-table)
    (define (dispatch m)
      (cond [(eq? m 'lookup-proc) lookup]
            [(eq? m 'insert-proc!) insert!]
            [(eq? m 'print-table) print-table]
            [else (error "Unkown opeartion -- TABLE" m)]))
    dispatch))

;; 3.26
; implement a binary tree records
; make-table: make a binary tree table
(define (make-table)
  (let ((local-table '*table*))
    ;; constructor, selector
    ; key-tree: return the key
    ; value-tree: return the value
    ; left-branch: return the left branch
    ; right-branch: return the right branch
    ; make-tree: make a tree (key, value, left, right)
    ; set-value-tree!: set value
    ; set-left-branch-tree!: set left branch
    ; set-right-branch-tree!: set right branch
    (define (key-tree tree)
      (car tree))
    (define (value-tree tree)
      (cadr tree))
    (define (left-branch tree)
      (caddr tree))
    (define (right-branch tree)
      (cadddr tree))
    (define (make-tree key value left right)
      (list key value left right))
    (define (set-value-tree! tree value)
      (set-car! (cdr tree) value))
    (define (set-left-branch-tree! tree left)
      (set-car! (cadr tree) left))
    (define (set-right-branch-tree! tree right)
      (set-car! (cdddr tree) right))

    ;; internal procedure
    ; lookup: look for the value of a given key
    ; insert: insert a new tree
    ; print-table: print the table
    ; dispatch: start a procedure
    (define (lookup key)
      (define (iter key tree)
        ; if key is empty => false
        ; if key is equal to the key of tree => value of the key
        ; if key is smaller than the key of tree
        ; => go to left branch
        ; if key if bigger than the key of tree
        ; => go to right branch
        (cond [(null? key) #f]
              [(= key (key-tree tree)) (value-tree tree)]
              [(< key (key-tree tree))
               (iter key (left-branch tree))]
              [(> key (key-tree tree))
               (iter key (right-branch tree))]))
      (iter key local-table))

    (define (insert key value)
      ; make a tree composed of the given key and value
      (define (make-branch key balue)
        (make-tree key value '() '()))
      ; 
      (define (iter key value tree)
        ; if tree is in the initial state, the
        ; => the tree would be the tree containing inserted key and value
        ; if the inserted key is equal to the key of the tree
        ; => update the value of the tree that has the key
        ; if the key is smaller
        ; .. if the left branch is empty
        ; .. => set the tree at the left branch
        ; => iterate the procedure from the left branch
        ; if the key is bigger
        ; .. if the right branch is empty
        ; .. => set the tree at the right branch
        ; => iterate the procedure from teh right branch
        (cond [(eq? tree '*table*)
               (set! local-table (make-branch key value))]
              [(= key (key-tree tree))
               (set-value-tree! tree value)]
              [(< key (key-tree tree))
               (if (null? (left-branch tree))
                   (set-left-branch-tree! tree (make-branch key value))
                   (iter key value (left-branch tree)))]
              [(> key (key-tree tree))
               (if (null? (right-branch tree))
                   (set-right-branch-tree! tree (make-branch key value))
                   (iter key value (right-branch tree)))]))
      (iter key value local-table)
      'ok)

    (define (print-table)
      (display local-table)
      (newline))

    (define (dispatch m)
      (cond [(eq? m 'print-table) print-table]
            [(eq? m 'lookup-proc) lookup]
            [(eq? m 'insert-proc!) insert!]
            [else (errror "Unknown operation TABLE" m)]))
    dispatch))

;; 3.27
;; memoization
;; Fibonacci numbers
; ordinary version
(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))nn
; memoized version
(define memo-fib
  (memoize
   (lambda (n)
     (cond [(= n 0) 0]
           [(= n 1) 1]
           [else (+ (memo-fib (- n 1))
                    (memo-fib (- n 2)))]))))
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))