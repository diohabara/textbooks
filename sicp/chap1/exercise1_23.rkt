(define (next n)
  (if (= n 2)
      3
      (+ n 2)))
; display runtime when testing prime numbers
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; test prime
(define (prime? n)
  (= n (smallest-divisor n)))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (next test-divisor))]))
(define (divides? a b) (= (remainder b a) 0))
(define (square x) (* x x))

; checks the primality of consecutive odd intergers
(define (search-for-primes start end)
  (if (even? start)
    (search-for-primes (+ start 1) end)
    (cond [(< start end)
          (timed-prime-test start)
          (search-for-primes (+ start 2) end)])))