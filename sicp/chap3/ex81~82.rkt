;; 3.81
; from text
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

; random-number-generator:
; if it takes as an argument 'generate, generate a sequence of random numbers
; if it takes as an argument 'reset, reset the sequence of random numbers
; requeset-stream: a stream of requests
(define (random-number-generator request-stream)
  (define s
    (cons-stream
     the-empty-stream
     (stream-map
      (lambda (request value)
        (cond [(eq? request 'generate) (rand-update value)]
              [(and (pair? request) (eq? (car request) 'reset))
               (cdr request)]
              [else (error "generate invalid request")]))
      request-stream
      s)))
  s)

;; 3.82

; from text
(define cesaro-stream
  (map-successive-pairs
   (lambda (r1 r2) (= (gcd r1 r2) 1))
   random-numbers))
(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

; monte carlo
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-straem
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (netx passed (+ failed 1))))

(define (random-in-range x1 x2)
  (+ x1 (rand (- x2 x1))))

; monte carlo integration
(define (estimate-integral p x1 x2 y1 y2)
  (stream-map (lambda (m) (* (- x2 x1) (- y2 y1) m))
              (monte-carlo
               (stream-map p
                           (stream-map (lambda (x) (random-in-range x1 x2)) integers)
                           (stream-map (lambda (x) (random-in-range y1 y2)) integers))
               0 0)))