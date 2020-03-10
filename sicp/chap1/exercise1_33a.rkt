;; Filtered Accumulation
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                    (filtered-accumulate filter combiner null-value term (next a) next b))
          (filtered-accumulate filter combiner null-value term (next a) next b))))

;; Sum of Squares of the prime numbers, from a to b
; Test Prime
(define (prime? n)
   (define (smallest-divisor n)
      (define (find-divisor n test-divisor)
         (define (next x)
            (if (= x 2) 3 (+ x 2)))
         (define (divides? a b)
            (= (remainder b a) 0))
         (cond ((> (square test-divisor) n) n)
               ((divides? test-divisor n) test-divisor)
               (else (find-divisor n (next test-divisor)))))
      (find-divisor n 2))
   (= n (smallest-divisor n)))
(define (square x) (* x x))
(define (inc x) (+ x 1))

; Procedure
(define (prime-square a b)
  (filtered-accumulate prime? + 0 square a inc b))
(prime-square 2 3) ; 13 = 4 + 9 =  2^2 + 3^2
(prime-square 2 10) ; 87 = 4 + 9 + 25 + 49 = 2^2 + 3^2 + 5^2 + 7 ^ 2