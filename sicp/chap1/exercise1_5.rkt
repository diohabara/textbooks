#lang racket
;; Exercise1-5
(define (p) (p))
(define (test x y)
	(if (= x 0) 0 y))

(test 0 (p))
;; if normal order...fully expand and reduce
;; (if (= x 0) 0 y)
;; (if (= 0 0) 0 p)
;; (if #t 0 p)
;; 0
;; if applicative order...evaluate the arguments and then apply
;; (if (= x 0) 0 y)
;; (if (= 0 0) 0 p)
;; (if (= 0 0) 0 p)
;; (if (= 0 0) 0 p)
;; it endlessly evaluate the arguments and never
