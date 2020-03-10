;; 3.7
; from exercise3.3
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
        (error "Incorrect Password")))
  dispatch)
; make-joint:
; arg1: password-protected account
; arg2: the password, with which the joint operatoin will proceed
; arg3: newly added password
(define (make-joint account old-password new-password)
  (define (dispatch p m)
    (if (eq? p new-password)
        (cond [(eq? m 'withdraw) (account old-password 'withdraw)]
              [(eq? m 'deposit) (account old-password 'deposit)]
              [else (error "Unknow-n requeset: JOINT-ACCOUNT"
                    m)])
        (error "Incorrect Password")))
  dispatch)

; for testing
(display "3.7\n")
(define peter-acc (make-account 100 'open-sesame))
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))
((peter-acc 'open-sesame 'withdraw) 10) ; 90
((paul-acc 'rosebud 'withdraw) 10) ; 80

;; 3.8
(define f
  (let ((cnt 1))
    (lambda (x)
      (set! cnt (* cnt x))
      cnt)))

; for testing
(display "3.8\n")
; from right to left
(f 1) ; 1
(f 0) ; 0
; => 1
; from left to right
(f 0) ; 0
(f 1) ; 0
; => 0