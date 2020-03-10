;; 3.15
; pic problem. skip

;; 3.16
; Ben's procedure to count the number of pairs
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))


; for test
(display "3.16\n")
(define x (list 'a 'b 'c))
(count-pairs x) ; 3

(define y1 (list 'b 'c))
(define y2 (list 'a))
(set-car! y1 y2)
(set-car! (cdr y1) y2)
(count-pairs y1) ; 4

(define z1 (list 'c))
(define z2 (list 'b))
(define z3 (list 'a))
(set-car! z2 z3)
(set-cdr! z2 z3)
(set-car! z1 z2)
(set-cdr! z1 z2)
(count-pairs z1) ; 7
 
(define non (list 'a 'b 'c))
(set-cdr! (cdr (cdr non)) non)
; (count-pairs non) ; nothing at all

;; 3.17
; write the correct version of count-pairs
(define (count-pairs x) 
  (let ((encountered '())) ; encountered contains a list that it encounterd
    (define (helper x)
      (if (or (not (pair? x)) (memq x encountered)) ; if x is not pair or x is in encountered, return 0
          0 
          (begin 
            (set! encountered (cons x encountered)) ; concatenate x and encountered
            (+ (helper (car x)) ; the first list of x will be recursed
               (helper (cdr x)) ; the rest of x will be recursed
               1)))) 
    (helper x)))

; for test
(display "3.17\n")
(define x (list 'a 'b 'c))
(count-pairs x) ; 3

(define y1 (list 'b 'c))
(define y2 (list 'a))
(set-car! y1 y2)
(set-car! (cdr y1) y2)
(count-pairs y1) ; 3

(define z1 (list 'c))
(define z2 (list 'b))
(define z3 (list 'a))
(set-car! z2 z3)
(set-cdr! z2 z3)
(set-car! z1 z2)
(set-cdr! z1 z2)
(count-pairs z1) ; 3

(define non (list 'a 'b 'c))
(set-cdr! (cdr (cdr non)) non)
(count-pairs non) ; 3

;; 3.18
; loop?: check whether lis has a loop
; if 
(define (loop? lis)
  (define record '())
  (define (in-list? x)
    (if (memq x record)
        #t
        (begin (set! record (cons x record))
               #f)))
  (define (iter-loop? item)
    (if (not (pair? item)) ; if x is not pair, false
        #f
        (if (in-list? (car item)) ; if (car item) is in list => true
            #t
            (iter-loop? (cdr item))))) ;  otherwise, iterate the procedure
  (iter-loop? lis))

; for test
(display "3.18\n")
(define x (list 'a 'b 'c))
(loop? x) ; #f

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))
(define (make-cycle x) (set-cdr! (last-pair x) x) x)
(define z (make-cycle (list 'a 'b 'c)))
(loop? z)  ; #t

;; 3.19
; here's Floyd's implementation(..This must be what it takes to be an algorithmer;<)
(define (contains-cycle? lst) 
   (define (safe-cdr l) 
     (if (pair? l) 
         (cdr l) 
         '())) 
   (define (iter a b) 
     (cond [(not (pair? a)) #f]
           [(not (pair? b)) #f]
           [(eq? a b) #t]
           [(eq? a (safe-cdr b)) #t]
           [else (iter (safe-cdr a) (safe-cdr (safe-cdr b)))])) 
   (iter (safe-cdr lst) (safe-cdr (safe-cdr lst)))) 
  
  
 ; Tested with mzscheme implementation of R5RS: 
 (define x '(1 2 3 4 5 6 7 8)) 
 (define y '(1 2 3 4 5 6 7 8)) 
 (set-cdr! (cdddr (cddddr y)) (cdddr y)) 
 (define z '(1)) 
 (set-cdr! z z) 
 x ; (1 2 3 4 5 6 7 8) 
 y ; (1 2 3 . #0=(4 5 6 7 8 . #0#)) 
 z ; #0=(1 . #0#) 
 (contains-cycle? x) ; #f 
 (contains-cycle? y) ; #t 
 (contains-cycle? z) ; #t 