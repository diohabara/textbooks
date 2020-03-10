;; 3.50
; complete the definition of stream-map analogous to map
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (mapy stream-cdr argstreams))))))

;; 3.51
; simply return its argument after printing it
(define (show x)
  (display-line x)
  x)

; what will be printed in response to evaluating each expression
(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))
(stream-ref x 5)
; => 1, 2, 3, 4, 5
(stream-ref x 7)
; => 6, 7

;; 3.52
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
(stream-ref y 7)
(display-stream z)

; what is the value of sum after all expressions?
; => 136
; because the first value is evaluated whatever the value is in stream-map
; so from the first element to the 7th even number is returned

; what is the printed response to evaluating the stream-ref and display-stream expressions?
; all number that can be divided by 5 will be printed

; would the result differ if the implementation of delay is not optimized by memo-proc?
; if we had not momoized the result, we would need to reevalute the result
; which in turn change the result