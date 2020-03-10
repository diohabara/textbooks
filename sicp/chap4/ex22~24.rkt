;; 4.22
; extend the evaluator in this section to support the special form let
; put this procedure in the analyze evaluator
((let exp) (analyze (let->combination exp)))

(define (let->combination exp)
  (if (pair? (car (let-clauses exp)))
      (expnad-let-clauses (let-clauses exp))
      (expand-named-let-clauses (let-clauses exp))))

;; 4.23
; a procedure in the text
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

; Alyssa's idea
(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond [(null? (cdr procs))
           ((car procs) env)]
          [else
           ((car procs) env)
           (execute-sequence (cdr procs) env)]))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (lambda (env)
      (execute-sequence procs env))))

; compare two versions of analyze-sequence
; what work will the execution proceduree by Alyssa do?
; calls execute-sequence at run time, which calls another procedure

; what about the one in the text
; generate the same procedure once

;; 2.24