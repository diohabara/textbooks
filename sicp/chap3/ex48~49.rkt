;; 3.48
; why can deadlock-avoidance method described above avoid deadlock?
; => if the proceeding step is detemined, it will not lock each other's accounts
; rewite serialized-exchange to incorporate this idea(plus make-account)
; from text
(define (exchange account1 account2)
  (let ((diffenrence (- (account1 'balance)
                        (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-account-and-serialized id balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set ! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond [(eq? m 'id) id]
            [(eq? m 'withdraw) withdraw]
            [(eq? m 'deposit) deposit]
            [(eq? m ''balance) balance]
            [(eq? m 'serializer) balance-serializer]
            [else (error "Unknown request -- MAKE-ACCOUNT"
                         m)]))
    dispatch))

(define (serializerd-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (let ((exchanger (if (> (account1 'id) (account2 'id))
                         ((serializer1 (serializer2 exchange))
                          account1
                          account2
                         ((serializer2 (serializer1 exchange))
                          account2
                          account1
                          )))))
      (exchanger account1 account2))))

;; 3.49
; give a scenario where the deadlock-avoidance mechanism described above does not work
; when there is a condition that a certain resource must be locked before
; another resource may be accessed, this mechanism does not work