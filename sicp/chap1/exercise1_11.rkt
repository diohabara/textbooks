; recursive
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))
            

; iterative
(define (f n)
  (if (< n 3)
      n
      (f_iter 2 1 0 n)))
(define (f_iter f-1 f-2 f-3 n)
  (if (= count 0)
      f-1
      (fib_iter (+ f-1 (* 2 f-2) (* 3 f-3)
                   f-1
                   f-2
                   (- n 1)))))