;; 2.53
; response
(display "2.53\n")
(list 'a 'b 'c) ; {mcons 'a {mcons 'b {mcons 'c '()}}}
(list (list 'george)) ; {mcons {mcons 'george '()} '()}
(cdr '((x1 x2) (y1 y2))) ; {mcons {mcons 'y1 {mcons 'y2 '()}} '()}
(cadr '((x1 x2) (y1 y2))) ; {mcons 'y1 {mcons 'y2 '()}}
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socks)) ; {mcons 'red {mcons 'shoes {mcons 'blue {mcons 'socks '()}}}}

;; 2.54
; define equal procedure that chekcks if two arguments are the same lists
(define (equal p1 p2)
  (cond [(and (null? p1) (null? p2)) #t]
        [(or (null? p1) (null?2 p2)) #f]
        [(and (pair? p1) (pair? p2))
         (and (equal? (car p1) (car p2))
              (equal? (cdr p1) (cdr p2)))]
        [(or (pair? p1) (pair? p2)) #f]
        [else (eq? p1 p2)]))

(display "2.54\n")
(equal? '(this is a list) '(this is a list)) ; #t
(equal? '(this is a list) '(this (is a) list)) ; #f

;; 2.55
; ' is syntax sugar of quote
; so ''brabra is the syntax sugar of (quote (quote (brabra)))
; and (car ''brabra) returns (quote (quote)) (= 'quote)
(car ''abracadabra)