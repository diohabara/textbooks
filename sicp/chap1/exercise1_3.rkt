#lang racket
(define (square x) (* x x))  ; square x
(define (sum-of-bigger-two a b c)
	(cond [(and (>= a b) (>= b c)) (+ (square a) (square b))]
		[(and (>= a c) (>= c b)) (+ (square a) (square c))]
		[else (+ (square b) (square c))]))
