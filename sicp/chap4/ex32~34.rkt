;; 4.32
; illustrate the difference between the streams of Chapter3
; and the lazier lazy lists described in this section
; -> lazier lazy list has a delayed car
; how can we take advantage of this extra laziness?
; we can implement lazy tree structure

;; 4.33
; modify the evaluator's treatment of quoted expressions
; so that quoted lists typed at the driver loop will produce true lazy lists

; put this into eval
((quoted? exp) (text-of-quotation exp env))

(define (text-of-quotation exp env)
  (if (list? (cadr exp))
      (eval (make-quotation-list (cadr exp)) env)
      (cadr exp)))

(define (make-quotation-list lis)
  (if (null? list)
      `()
      (let ((first-list (car list))
            (rest-list (cdr list)))
        (list `cons (list `quoted first-list)
              (make-quotation-list rest-lists)))))

;; 4.34
; modify the driver loop for the evaluator so that lazy pairs and lists will print
; in some reasonable way
; -> skip

; modify the representation of lazy pairs so that the evaluator can idenfity them
; in order to pirnt them
; -> skip