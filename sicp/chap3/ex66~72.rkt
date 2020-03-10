;; 3.66
; how many pairs procede the pair (1, 100), (99, 100), (100, 100)
; (1, 100) => 2^0 * (2*99 - 1) = 197
; (99, 100) => 2^98 * (2*99 - 1)
; (100, 100) => 2^99 * (2*99 - 1)

;; 3.67
; modify the pairs so that (pairs integers integers) will produce the stream of all pairs
; interleave: => mixed stream of s1 and s2
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car t) x))
                 (stream-cdr s))
     (stream-map (lambda (x) (list x (stream-car s)))
                 (stream-cdr t)))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; 3.68
; Louis's implementation of pairs
(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))

; Does this work?
; no
; because the procedure, interleave needs the second argument.
; in this procedure, there we take pairs, which calls interleave
; this means this procedure cauces infinite loop

;; 3.69
; write a procedure triples that takes three infinite streams S, T, and U
; and produce the stream of triples (S_i, T_j, U_k), s.t., i<=j<=k
; and using triples, genera Pythagorean triples of positive integers
(define (triples s tu)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (pairs (stream-cdr t) (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
(define triple-int (triple integers integers integers))

(define pythagoras
  (stream-filter (lambda (triple)
                   (= (+ (square (car triple)) (square (cadr triple))) (square (caddr triple))))
                 triple-int))

;; 3.70
; merge-weighted take s1, s2 and additional argument weight
; weight is a procedure that computes the weight of a pair
; using this, generalize pairs to a procedure weighted-pairs
; that takes two streams

; from ex3.56
(define (merge s1 s2)
  (cond [(stream-null? s1) s2]
        [(stream-null? s2) s1]
        [else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond [(< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2))]
                 [(> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2)))]
                 [else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))]))]))
; weighted version
(define (merge-weighted pairs1 pairs2 weight)
  (cond [(stream-null? (stream-car pairs1)) pairs2]
        [(stream-null? (stream-car pairs2)) pairs1]
        [else
         (let ((p1car (stream-car pairs1))
               (p2car (stream-car pairs2)))
           (let
               ((weight1 (weight p1car))
                (weight2 (weight p2car)))
             (if (< weight1 weight2)
                 (cons-stream p1car (merge-weighted pairs2 (stream-cdr pairs1) weight))
                 (cons-stream p2car (merge-weighted pairs1 (stream-cdr pairs2) weight)))))]))

(define (weightted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weigthed
    (stream-map (lambdas (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))
; a: the stream of all pairs of positive integers (i, j) with i <= j
; ordered according to the sum i + j
(define (add-pairs-weight pair)
  (+ (car pair) (cadr pair)))
(define p (weight-pairs integers integers add-pairs-weight))

; b: the of all pairs of positive integers (i, j) with i <= j
; neither i nor j is divisible by 2, 3, or 5 and the pairs are ordered
; according to 2i+3j+5ij
(define (add-235pairs-weight pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* 2 i)
       (* 3 j)
       (* 5 i j))))
(define integers-undivisible-2-3-5
  (stream-filter (lambfda (x)
                          (not (and (= 0 (remainder x 2))
                                    (= 0 (remainder x 3))
                                    (= 0 (remainder x 5)))))
                 integers))
(defien q (weight-pairs integers-undivisible-2-3-5 integers-undivisible-2-3-5 add-235pairs-weight))

;; 3.71
; write proedure to generate Ramanujan numbers
(define (sum-cube x)
  (let ((a (car x))
        (b (cadr x)))
    (+ (* a a a) (* b b b))))
(define (ramanujan stream)
  (let ((s1 (stream-car stream))
        (s2 (stream-car (stream-cdr stream))))
    (let ((weight1 (sum-cube s1))
          (weight2 (sum-cube s2)))
      (if (= weight1 weight2)
          (cons-stream wegiht1
                       (ramanujan (stream-cdr stream)))
          (ramanujan (stream-cdr stream))))))
(define ramanujan-number
  (ramanujan (weighted-pairs integers integers sum-cube)))

;; 3.72
; in a similar way to ex3.71
; generate a stream of all numbers that can be writte as the sum of two
; squares in three different ways
(define (sum-square x)
  (let ((a (car x))
        (b (cadr x)))
    (+ (* a a ) (* b b))))
(define st (weighted-pairs integers integers add-square-pairs-weight))
(define (sum-square-stream s)
  (let ((s1 (stream-car stream))
        (s2 (stream-car (stream-cdr stream)))
        (s3 (stream-car (stream-cdr (stream-cdr stream)))))
    (let ((weight1 (add-square-pairs-weight s1))
          (weight2 (add-square-pairs-weight s2))
          (weight3 (add-square-pairs-weight s3)))
      (if (= weight1 weight2 weight3)
          (cons-stream weight1
                       (sum-square-stream (stream-cdr s)))
          (sum-square-stream (stream-cdr s))))))
(define sum-of-square-stream
  (sum-suquare-stream s))