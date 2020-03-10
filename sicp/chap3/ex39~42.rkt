;; 3.39
; what of five possibiliities in th parallel execution shown above reamin?
; in this serialize execution
(define x 10)
(define s (make-serializer))
(parallel-execute
 (lambda () (set! x ((s (lambda () (* x x)))))) ; p1(double, set)
 (s (lambda () (set! x (+ x 1))))) ; p2
; => 100(double, p2(11), set(100)), 101(double, set, p2), 121(p2, double, set)

;; 3.40
; all possible values of x that can result from executing
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x x x))))
; => 10, 10^3, 10^4, 10^5 10^6

; which of these possibilities remain
; in this serialize execution
(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))
; => 10^6

;; 3.41
; Ben's implementation of bank account
; this is based on his concern that unserialized access to the bank balance
; can result in anomalous behavior
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond [(eq? m 'withdraw) (protected withdraw)]
            [(eq? m 'deposit) (protectd deposit)]
            [(eq? m 'balance)
             ((protected
               (lambda () balance)))] ; serialized
            [else
             (error "Unknown request: MAKE-ACCOUNT"
                    m)]))
    dispatch))

; no need.
; reason, it just returns the value. it does not set any value

;; 3.42
; an implementation by Ben, that he think is safer version
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond [(eq? m 'withdraw) protected-withdraw]
              [(eq? m 'deposit) protected-deposit]
              [(eq? m 'balance) balance]
              [else
               (error "Unknown request: MAKE-ACCOUNT"
                      m)]))
      dispatch)))
; Is this safe?
; => safe
; Is there any difference in what concurrency is allowed by these two versions?
; => the same, the only difference is the new calls a extra function for withdraw and deposit