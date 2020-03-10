;; 2.61
; improved adjoin-set, which require half as many steps
(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(= x (car set)) #t]
        [(< x (car set)) #f]
        [else (element-of-set? x (cdr set))]))
(define (adjoin-set x set)
  (cond [(null? set) (cons x '())]
        [(= x (car set)) set]
        [(< x (car set)) (cons x set)]
        [(> x (car set)) (cons (car set)
                               (adjoin-set x (cdr set)))]))

;; 2.62
; improved union-set whose speed is twice higher
(define (union-set set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [(= (car set1) (car set2))
            (cons (car set1) (union-set (cdr set1) (cdr set2)))]
        [(< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) set2))]
        [else (cons (car set2) (union-set set1 (cdr set2)))]))
      
