; row: the nth from the top of the triangle
; colmun: the nth from the left of the the same row

(define (pascal_tri row column)
  (if (or (= column 1)
          (= column row))
      1
      (+ (pascal_tri (- row 1) (- column 1))
         (pascal_tri (- row 1) column))))
          