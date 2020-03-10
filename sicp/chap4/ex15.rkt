;; 4.15
; p is said to halt on 'a if evaluating the expression (p a)
; returns a value, #t or #f

; show that it is impossible to write a procedure halts?
; that correctly determines whether p halts on a 
(define (run-forever) (run-forever))
(define (try p)
  (if (halts? p p) (run-forever) 'halted))

(try try)
; suppose this halts, then (halts? try try) is true,
; then (try p) returns (run-forever)
; this result contradicts to the fact (try p) halts

; suppose this does no halt, then (halts? try try) is false,
; then  (try p) returns 'halted
; this result contradicts to the fact (try p) runs

; therefore, halts? cannot be defined