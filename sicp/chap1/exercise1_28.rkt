;; Miller-Rabin test
(define (expmod base exp n)
  (cond [(= exp 0) 1]
        [(even? exp)
         (square-check (expmod base (/ exp 2) n) n)]
        [else
         (remainder (* base (expmod base (- exp 1) n))
                    n)]))

(define (square-check x n)
  (if (and (not (or (= x 1) (= x (- n 1))))
           (= (remainder (* x x) n) 1))
      0
      (remainder (* x x) n)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 2 (random (- n 2)))))