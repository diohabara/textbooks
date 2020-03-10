;; 3.63
; sqrt-stream procedure
; text version
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)
; Louis version 
(define (sqrt-stream x)
  (cons-stream 1.0 (stream-map
                    (lambda (guess)
                      (sqrt-improve guess x))
                    (sqrt-stram x))))

; Louis version is less efficient
; Reason: Loius version needs to recreate the whole sequence at every calculation

; what if we do not use the optimization for delay
; without memoirization, the inefficiency is the same

;; 3.64
; write a procedure stream-limit that takes as arguments a stream and a number
; it should examine the stream until it finds two successive elements that
; differ in absolute value by less than the tolerance
(define (stream-limit s tolerance)
  (let ((s1 (stream-car s))
        (s2 (stream-car (stream-cdr s))))
    (if (< (abs (- s1 s2)) tolerance)
        s2
        (stream-limit (stream-car s) tolearnce))))

; here is a procedure that uses this procedure
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

; 3.65
(define (lg-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (lg-summands (+ n 1)))))
(define (lg-stream
         (partial-sums (lg-summands 1)))

  