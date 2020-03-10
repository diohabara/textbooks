;; 2.24
(list 1 (list 2 (list 3 4)))

; result printed by the interpreter
; {mcons 1 {mcons {mcons 2 {mcons {mcons 3 {mcons 4 '()}} '()}} '()}}
; I do not describe box-and-pointer structure & tree here

;; 2.25
; first I wrote down the tree structure of
; (1 3 (5 7) 9)
; ((7))
; (1 (2 (3 (4 (5 (6 7))))))
; on the note, and solved the problems
; extract 7
(display "2.25\n")
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car (list (list 7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr
                                                        (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

;; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(display "2.26\n")
(append x y) ; {mcons 1 {mcons 2 {mcons 3 {mcons 4 {mcons 5 {mcons 6 '()}}}}}}
(cons x y) ; {mcons {mcons 1 {mcons 2 {mcons 3 '()}}} {mcons 4 {mcons 5 {mcons 6 '()}}}}
(list x y) ; {mcons {mcons 1 {mcons 2 {mcons 3 '()}}} {mcons {mcons 4 {mcons 5 {mcons 6 '()}}} '()}}

;; 2.27
(define x (list (list 1 2) (list 3 4)))

(display "for check\n")
; normal reverse
(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items))
              (cons (car items) nil))))
(reverse x) ; {mcons {mcons 3 {mcons 4 '()}} {mcons {mcons 1 {mcons 2 '()}} '()}}
; deep-reverse
(define (deep-reverse items)
  (cond [(null? items) nil]
        [(pair? (car items))
         (append (deep-reverse (cdr items))
                 (list (deep-reverse (car items))))]
        [else
         (append (deep-reverse (cdr items))
                 (list (car items)))]))
(deep-reverse x) ; {mcons {mcons 4 {mcons 3 '()}} {mcons {mcons 2 {mcons 1 '()}} '()}}

;; 2.28
(define (fringe tree)
  (cond [(null? tree) nil]
        [(not (pair? list)) (list tree)]
        [else (append (fringe (car tree))
                      (fringe (cdr tree)))]))
(define x (list (list 1 2) (list 3 4)))

; for testing
(display "2.28\n")
(fringe x)
(fringe (list x x))

;; 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

; a: selectors
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))
(define (branch-length branch)
   (car branch))
(define (branch-structure branch)
   (car (cdr branch)))

; b: return the total weight of a mobile
(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))
(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-branch (branch-structure branch))
      (branch-structure branch)))

(define (total-weight mobile)
  (+ (total-weight (left-branch branch))
     (total-weight (right-branch branch))))

; c: test whether a binary mobile is balanced
(define (branch-balanced? branch)
  (if (pair? (branch-structure branch))
      (branch-balanced? (branch-structure branch))
      #t))

; d: conversion to new programs
; new programs
(define (make-mobile left right) (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define (right-branch mobile)
  (cdr mobile))
(define (left-branch mobile)
  (car mobile))

                    