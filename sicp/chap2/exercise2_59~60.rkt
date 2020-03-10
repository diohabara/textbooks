;; 2.59
; union-set
(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(equal? x (car set)) #t]
        [else (element-of-set? x (cdr set))]))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (union-set set1 set2)
    (cond [(null? set1) set2]
          [(null? set2) set1]
          [(element-of-set? (car set1) set2) (union-set (cdr set1) set2)]
          [else (cons (car set1) (union-set (cdr set1) set2))]))

; for testing
(display "2.59\n")
(union-set '(1 2 3 4) '(1 2 5 6 7))

; 2.60
; implement set which is allowed to duplicate the same numbers
; how is the efficiency:
; element-of-set: O(n)->O(n)
; adjoin-set: O(n)->O(1)
; intersection-set: O(n^2)->O(n^2)
; union-set: O(n-2)->O(n)
(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(equal? x (car set)) #t]
        [else (element-of-set? x (cdr set))]))
(define (adjoin-set x set)
  (cons x set))
(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2)) '()]
        [(element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2))]
        [else (intersection-set (cdr set1) set2)]))
(define (union-set set1 set2)
  (append set1 set2)