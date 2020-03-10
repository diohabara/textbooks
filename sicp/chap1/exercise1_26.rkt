;; Louis Reasoner's code
(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m)]
        [else
         (reaminder (* base
                       (expmod base (- exp 1) m))
                    m)]))
; By rewriting "square",
; this procedure have to calculate "expmod" twice,
; which means that transforms an  O(logn) procedure into an O(n) procedure.