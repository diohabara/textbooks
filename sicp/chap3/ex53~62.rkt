
;; 3.53
; describe the elements of the stream define by
(define s (cons-stream 1 (add-streams s s)))
; the first element is 1, then after that is the numbers 2 powered by n - 1

;; 3.54
; define a procedure mul-streams analogous to add-streams
; which produces the product of its two input streams
(define (mul-streams s1 s2)
  (stream- * s1 s2))

; define factorials
(define factorials
  (cons-stream 1 (mul-streams factorials
                              (add-streams ones integers))))

;; 3.55
; define partial-sums
; arg: stream, s
; return: stream whose elements are s0, s0+s1, s0+s1+s2
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partila-sums s))))

;; 3.56
; S is the stream of numbers
; S begins with 1
; the elements of (scale-stream S 2) are also elements of S
; the same is true for (scale-stream S 3) and (scale-stream 5 S)
; these are all the elements of S
; merge: combines two ordered streams into one ordered result, eliminating repetitions
(define (merge s1 s2)
  (cond [(stream-null? s1) s2]
        [(stream-null? s2) s1]
        [else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond [(< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2))]
                 [(> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2)))]
                 [else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))]))]))

; the required stream may be constructed with merge
(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

;; 3.57
; how many additions are performed when we compute n-th fibonacci number
; with fibs based on the add-streams?
; => n-1 times. O(n)
; because it uses the results already calculated
; without memoization, we need to calculat from scratch each time
; in order words, the order is O(n^2)

;; 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

; (expand 1 7 10)
; => this result is 10 / 7
; (expand 3 8 10)
; => this result is 30 / 8

;; 3.59
; here we represent infinite streams
;; a:
; integrate-series, the argument are stream a0, a1, a2...
; return the a0, a1/2, a3/3...
(define (integrate-series s)
  (stream-map / s integers))
;; b:
; here is the series for e^x
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

; cosine = the derivative of sine
; sine = the negative derivative of cosine
; here are the definitions of sine and cosine
(define cosine-series
  (cons-stream 1 (stream-map - (integrate-series sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))n

;; 3.60
; define multiplication series of s1 and s2
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-cdr s1))
                            (mul-series (stream-cdr s1) s2))))

;; 3.61
; S power series whose constatnt term is 1
; invert-unit-series: computes 1/S for a power series S with constant term 1
(define (invert-unit-series s)
  (stream-cons 1 (scale-stream (mul-series (stream-cdr s) (inverted-unit-series s))
                               (-1))))

;; 3.62
; using the result of 3.60 and 3.61
; define a procedure div-series that divides 2 power series
; div-seires should work for any two series
; provided that denominator series begins with a nonzero constant term
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "divided by zero: DIV-SERIES")
      (mul-series s1 (invert-unit-series s2))))

; plus show how to use div-seires, to generate the power series for tangent
(define tan-series
  (div-series sine-series cosine-series))