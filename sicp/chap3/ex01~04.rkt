;; 3.1
; make-accumulator: generates accumulato, returns sum of its arguments
; take 1 argument at each time, and
(define (make-accumulator sum)
  (lambda (amount)
    (begin (set! sum (+ sum amount)))
    sum))

; for testing
(display "3.1\n")
(define A (make-accumulator 5))
(A 10) ; 15
(A 10) ; 25

;; 3.2
; make-monitored: takes as input a procedure, f, that takes 1 input
(define (make-monitored f)
  (let ((times 0))
    (define (dispatch mf)
      (cond [(eq? mf 'how-many-calls?) times]
            [(eq? mf 'reset-count) (begin (set! times 0))
                                   times]
            [else (begin (set! times (+ times 1))
                         (f mf))]))
    dispatch))

; for testing
(display "3.2\n")
(define s (make-monitored sqrt))
(s 100) ; 10
(s 'how-many-calls?) ; 1
(s 'reset-count) ; 0

;; 3.3
; make-account: creates password-protected accounts
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
        (cond [(eq? m 'withdraw) withdraw]
              [(eq? m 'deposit) deposit]
              [else (error "Unknown request: MAKE-ACCOUNT"
                           m)])
        (error "Incorrect password")))
  dispatch)

; for testing
(display "3.3\n")
(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) ; 60
; ((acc 'some-other-password 'deposit) 50) ; Incorrect password

;; 3.4
; make-account: if incorrect password is called more than 7 times,
; call cops
(define (make-account balance password)
  (let ((incorrect-cnt 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops dummy-arg)
      (begin (set! incorrect-cnt (+ incorrect-cnt 1))
             (if (> incorrect-cnt 7)
                 "Calling police!"
                 "Incorrect password")))
    (define (dispatch p m)
      (if (eq? p password)
          (cond [(eq? m 'withdraw) withdraw]
                [(eq? m 'deposit) deposit]
                [else (error "Unknown request: MAKE-ACCOUNT"
                             m)])
          call-the-cops))
    dispatch))

; for testing
(display "3.4\n")
(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) ; 60
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; Incorrect password
((acc 'some-other-password 'deposit) 50) ; Incorrect password