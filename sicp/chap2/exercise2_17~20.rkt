;; 2.17
; return the last element of the list
(define (last-pair items)
  (if (null? (cdr items))
      (list (car items))
      (last-pair (cdr items))))

; for testing
(display "2.17\n") ; {mcons 34 '()}
(last-pair (list 23 72 149 34))

;; 2.18
; return reversed list
(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items))
              (cons (car items) nil))))

; for testing
(display "2.18\n")
(reverse (list 1 4 9 16 25)) ; {mcons 25 {mcons 16 {mcons 9 {mcons 4 {mcons 1 '()}}}}}

;; 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

; rewrite first-denomination, except-first-denomination, and no-more? in terms of list
(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))
(define (cc amount coin-values)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (no-more? coin-values)) 0]
        [else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values))]))

; for testing
(display "2.19\n")
(cc 100 us-coins) ; 292

;; 2.20
; return list of arguments that have the same even-odd parity
; as the first argument
(define (same-parity a . z)
  (define (iter items ans)
    (if (null? items)
        ans
        (iter (cdr items)
              (if (= (remainder (car items) 2)
                     (remainder a 2))
                  (append ans (list (car items)))
                  ans))))
  (iter z (list a)))

; for testing
(display "2.20\n")
(same-parity 1 2 3 4 5 6 7) ; {mcons 1 {mcons 3 {mcons 5 {mcons 7 '()}}}}
(same-parity 2 3 4 5 6 7) ; {mcons 2 {mcons 4 {mcons 6 '()}}}