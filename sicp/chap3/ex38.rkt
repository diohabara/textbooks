;; 3.38
(define balance 100)
;Peter: (set! balance (+ balance 10))
;Paul: (set! balance (- balance 20))
;Mary: (set! balance (- balance (/ balance 2)))
;; a
; Peter => Paul => Mary
; result: 45
; Peter => Mary => Paul
; result: 35
; Paul => Peter => Mary
; result: 45
; Paul => Mary => Peter
; result: 50
; Mary => Peter => Paul
; result: 40
; Mary => Paul => Peter
; result: 40
 
;; b
; skip