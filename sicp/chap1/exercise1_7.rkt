#lang racket
;; with small numbers, the gap between two numbers is already small, so the 'good-enough?'`s calculation error is too big.
;; with large numbers, dividing the numbers causes some round error. if those numbers are large enough, the error cannot be missed.
;; the next procedure is watching the change of value, which is better for the previous 'good-enough?'
(define (sqrt-iter guess x)
	(if (better-enough? guess x)
		guess
		(sqrt-iter (improve guess x) x)))
(define (improve guess x)
	(average guess (/ x guess)))
(define (average x y)
	(/ (+ x y) 2))
(define (better-enough? after before)
	(< (abs (- before after)) 1e-8))
|# for refference
(define (good-enough? guess x)
	(if (abs (- (square guess) x)) 0.001))
#|