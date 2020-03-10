;; 2.21
; return square of list
(define (square x) (* x x))
; without map
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))
; for testing
(display "2.21 1\n")
(square-list (list 1 2 3 4)) ; {mcons 2 {mcons 4 {mcons 6 '()}}}
; with map
(define (square-list items)
  (map square items))
; for testing
(display "2.21 2\n")
(square-list (list 1 2 3 4)) ; {mcons 2 {mcons 4 {mcons 6 '()}}}

;; 2.22
; iterative square-list(that returns a reversed list)
; reason: it return the last result in the iterative procedure
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))
; for testing
(display "2.22 1\n")
(square-list (list 1 2 3 4))

; improved one(that doesn't work)
; reason: it first returns nil, or an empty list
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

; for testing
(display "2.22 2\n")
(square-list (list 1 2 3 4))

;; 2.23
; the right result
(display "2.23: right\n")
(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))

#|

57
321
88
|#

; an implementation of for-each
(define (for-each proc items)
  (cond [(null? items) #t]
        [else (proc (car items))
              (for-each proc (cdr items))]))

; for testing
(newline)
(display "2.23: test\n")
(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))