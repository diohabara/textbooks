;; 2.66
(define (key record) (car record))
(define (data record) (cdr record))
(define (make-record key data) (cons key data))
(define (entry tree) (car tree))
(define (left-branch tree (cadr tree)))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (list->tree elements)
  (car (partial elements (length elements))))
(define (partial-tree elets n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (lookup given-key tree-of-records)
  (cond [(null? tree-of-records) #f]
        [(= given-key (key (car tree-of-records)))
         (car tree-of-records)]
        [(< given-key (key (car tree-of-records)))
         (lookup given-key (left-branch tree-of-records))]
        [(> given-key (key (car tree-of-records)))
         (lookup given-key (right-branch tree-of-records))]))
            