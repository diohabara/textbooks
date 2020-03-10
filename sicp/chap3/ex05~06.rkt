;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader{lib "htdp-intermediate-lambda-reader.ss" "lang"}{{modname ex05~06} {read-case-sensitive #t} {teachpacks ()} {htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)}}
;; 3.5
; implement Monte Carlo integration

(define random-init 12345)

(define (rand-update x)
  (modulo (+ (* 214013 x) 253011) 32767))

; rand: returns random number, with a random-init, and change x by rand-update
; rando-in-range: return a random number ranging from low to high
(define (random-in-range low high)
  (define rand (let ((x random-init))
                 (lambda ()
                   (set! x (rand-update x))
                   x)))
  (let ((range (- high low)))
    (+ low (* (random) range))))

; square: return square
(define (square x) (* x x))

; estimate-integral: return the proportion of circle in rectangle
; predicate: check if the (x, y) is in the circle
; x1, x2, y1, x2: bounds of a rectangle
(define (estimate-integral pred x1 x2 y1 y2 trials)
  (define (test)
    (pred  (random-in-range x1 x2) ; x1 <= x <= x2
           (random-in-range y1 y2) ; y1 <= y <= y2
           x1 x2 y1 y2)) ; low-x, high-x, low-y, high-y
 (monte-carlo trials test))

; monte-carlo: return 6/pi^2
; the result is the probablity of chosen two integers that have factors in common
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond [(= trials-remaining 0)
           (/ trials-passed trials)]
          [(experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1))]
          [else
           (iter (- trials-remaining 1) trials-passed)]))
  (iter trials 0))

; in-range?: if (x, y) is in a circle
; centered at {(high-x + low-x)/2, (high-y + low-y)/2}
; return 1
(define (in-range? x y low-x high-x low-y high-y)
  (let ((r (/ (- high-x low-x) 2)))
    (<= (+ (square (- x (/ (+ high-x low-x) 2)))
           (square (- y (/ (+ high-y low-y) 2))))
        (square r))
    1))
; for testing
(display "3.5\n")
(estimate-integral in-range? 0 2.0 0 2.0 1000)

;; 3.6
(define rand
  (let ((x random-init))
    (define (dispatch m)
      (cond [(eq? m 'generate)
             (begin (set! x (rand-update x))
                    x)]
            [(eq? m 'reset)
                  (begin (set! x random-init))
                  "reset is completed"]))
    dispatch))

; for testing
(rand 'generate)
(rand 'generate)
(rand 'reset)
(rand 'generate)