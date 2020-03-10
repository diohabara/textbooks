;; 3.43
; pic => skip

;; 3.44
; Ben's procedure
(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

; Louis claims that there is a problem here.
; and we need to use a more sophisticated method
; Is Louis right?
; => no
; if we want to do some actions to a shared account, it should be serialized
; however, if the account is not shared, the actions has already been serialized
; so no need to use anouther method


;; 3.45
; Louis redefine accounts as follows
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond [(eq? m 'withdraw) (balance-serializer withdraw)]
            [(eq? m 'deposit) (balance-serializer deposit)]
            [(eq? m 'balance) balance]
            [(eq? m 'serializer) balance-serializer]
            [else (error "Unknown request: MAKE-ACCOUNT" m)]))
    dispatch))
; deposits are handled as with the original make-account
(define (deposit account amount)
  ((account 'deposit) amount))
; what is wrong with Louis?
; => if procedures are automatically serialized, we cannot make a more
; high level procedure
; what happens when serialized-exchange is called
; => suppose we made 2 accounts, then we call one of the two
; then the account call the other account
; but the account has already protected because of the serialization
; so the procedure never ends