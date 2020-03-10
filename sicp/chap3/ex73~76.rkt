;; 3.73
; from text
; integrator of signals
(define (integral integrand initial-value dt)
  (define int
    (cons-stram initial-value
                (add-streams (scale-stream integrand dt)
                             int)))
  int)
; difinition of RC circuit
(define (RC r c dt)
  (define (rc i v0)
    (add-streams
     (stream-map (lambda (x) (+ (* x r) v0)) i)
     (scale-stream (integral i v0 dt) (/ 1 c))))
  rc)

;; 3.74
; sense-data: signal from center
; zero-crossing: corresponding zero crossings
; sign-change-detector: take 2 values as arguments and compare the values
; to produce an appropriate 0, 1, or -1
(define (sign-change-detector a b)
  (cond [(and (> a 0) (< b 0)) -1]
        [(and (< a 0) (> b 0))  1]
        [else 0]))

; make-zero-crossings; stream
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector
    (stream-car input-stream)
    last-value)
   (make-zero-crossings
    (stream-cdr input-stream)
    (stream-car input-stream))))
(define zero-crossings
  (make-zero-crossings sense-data 0))

; here is the equivalent version and more generalized one
(define zero-crossings
  (stream-map sign-change-detector
              senda-data
              (stream-cdr sense-data)))

;; 3.75
; Alyssa tried to extract noisy signals from sensor
; Louis' program which is a modified version of Alyssa
(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream
     (sign-change-detector avpt last-value)
     (make-zero-crossings
      (stream-cdr input-stream) avpt))))

; this does not correctly implement Alyssa's plan
; fix it without changing the structure
; the bug's cause is 
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stram-car input-stream)
                    last-value)
                 2)))
    (cons-stream
     (sign-change-detector avpt last-avpt)
     (make-zero-crossing (stream-cdr input-stream)
                         (stream-car input-stream)
                         (/ (+ (stream-car input-stream) last-value) 2)))))

;; 3.76
; smooth: takes a stream as input and produces a stream
; in which each element is the average of two successive input stream elements
(define (smooth input-stream)
  (define (iter s)
    (cons-stream (/ (+ (stream-car s)
                       (stream-car (stream-cdr s))) 2)
                 (iter (stream-cdr s))))
  (cons-stream 0 (iter input-stream)))
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))
(define (zero-crossings (make-zero-crossings (smooth sense-data) 0)))

