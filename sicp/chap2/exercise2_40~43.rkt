;; 2.40
; from text
; prime
(define square (lambda (x) (* x x)))
(define (divides? a b) (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n ) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (+ test-divisor 1))]))
(define (smallest-divisor n) (find-divisor n 2))
(define (prime? n)
  (= n (smallest-divisor n)))

; filter sequence
(define (filter predicate sequence)
  (cond [(null? sequence) nil]
        [(predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence)))]
        [else (filter predicate (cdr sequence))]))

; accumulate sequence
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; generage sequence of integers
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

; combination of mapping and accumulating with append
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; predicate of prime number
(define (prime-sum? pair)
      (prime? (+ (car pair) (cadr pair))))

; make-pair of two integers and the sum of them
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; make pairs, which has i, j, whre 1 <= j < i <= n
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

; redefinition of prime-sum-pairs
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

; for testing
(display "2.40\n")
(prime-sum-pairs 6)

;; 2.41
; nested 3 times
(define (order-triple n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; sum of three integers
(define (triple-sum? triple s)
  (= s (accumulate + 0 (triple))))

; make pair of three integers and its sum
(define (make-triple-sum triple)
  (append triple (list (accumulate + 0 triple))))

; procedure
(define (order-triple-sum n s)
  (define (triple-sum? triple) ; accumulate is for double, so this procedure is necessary
    (= s (accumulate + 0 triple)))
  (map make-triple-sum
       (filter triple-sum?
               (order-triple n))))

; for testing
(display "2.41\n")
(order-triple-sum 5 10)

;; 2.42
; rest-of-queen: a way to place k-1 queens in the 1st k-1 columns
; new-row: a proposed row in which to place the queen for the k-th column
; adjoin-position: adjoin a new row-column position to a set of positions
; safe?: a new queen in the k-th columns is safe
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
; procedures about position
(define (make-position row col)
  (cons row col))

(define (position-row position)
  (car position))

(define (position-col position)
  (cdr position))

(define empty-board nil)

(define (adjoin-position row col positions)
  (append positions (list (make-position row col))))

; check if queen is safe
; from text
; return n-th element
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

; we can ignore column because we place one queen in one column
(define (safe? col positions)
  ; kth-queen: kth queen's position
  ; other-queens: not kth queen's position from positions
  (let ((kth-queen (list-ref positions (- col 1)))
        (other-queens (filter (lambda (q)
                                (not (= col (position-col q))))
                              positions)))
    ; is new queen attckable?
    (define (attacks? q1 q2)
      (or (= (position-row q1) (position-row q2))
          (= (abs (- (position-row q1) (position-row q2)))
             (abs (- (position-col q1) (position-col q2))))))
    ; iterate until no kth queens can be considered or no board remained
    (define (iter q board)
      (or (null? board)
          (and (not (attacks? q (car board)))
               (iter q (cdr board)))))
    (iter kth-queen other-queens)))

; for testing
(display "2.42\n")
(queens 4)

;; 2.43
; Louis Reasoner's answer
; In the original code, queen-cols is called just once in each column
; However, using this procedure, the queen-cols is called for each row of the
; kth-column, so the new procedure will generate all the possible solutions for the
; first k-1 columns for each one of these rows
; Tree-recursive process takes T^board-size time to execute
(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))