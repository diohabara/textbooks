;; Better make-rat that normalizes the sign
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (- (/ n g)) (- (/ d g)))
        (cons (/ n g) (/ d g)))))