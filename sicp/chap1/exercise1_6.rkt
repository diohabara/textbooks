#lang racket
;; Exercise1-6
;; new-if statement is evaluated by applicative-order by which it endlessly evaluate the new-if
#| (define (sqrt-iter guess x)
	(new-if (good-enough? guess x)
			guess
			(sqrt-iter (improve guess x) x)))
in the second 'sqrt-iter', 'new-if' will come up again, and then after in the 'new-if' 'sqrt-iter' appears and, it will endlessly loop.
|#