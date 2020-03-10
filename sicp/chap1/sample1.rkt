(define (square n)
  (* n n))
;; 1.2.2 Tree recursion

; recurcive fib
(define (fib1 n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib1 (- n 1))
                 (fib1 (- n 2))))))

; iterative fib
(define (fib2 n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

; recurcive counting money
(define (count-change amount) (cc amount 5)) 
(define (cc amount kinds-of-coins)
  
  (cond ((= amount 0) 1) ; if amount if 0, it's over
        ((or (< amount 0) (= kinds-of-coins 0)) 0) ; ; if amount is minus, the coins can't be used anymore
        (else (+ (cc amount
                     (- kinds-of-coins 1)) ; use a smaller coin instead
                 (cc (- amount
                        (first-denomination
                         Kinds-of-coins))
                     kinds-of-coins))))) ;; subtract the coin value from amount
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; 1.2.4 Exponentiation

; Recurcive Exponentiation
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

; Iterative Exponetiation
(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

; Square Exponetiation
(define (fast-expt b n)
  (cond [(= n 0) 1]
        [(even? n) (square (fast-expt b (/ n 2)))]
        [else (* b (fast-expt b (- n 1)))]))

;; 1.2.5 Greatest Common Divisors
; Euclid's Algorithm
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; 1.2.6 Testing for Primality
; Simple test
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n ) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (+ test-divisor 1))]))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (square x) (* x x))

; Fermat test
(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m)]
        [else
         (remainder
          (* base (expmod base (- exp 1) m))
          m)]))

; Random Fermat test
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

; Fermat test(given times)
(define (fast-prime? n times)
  (cond [(= times 0) true]
        [(fermat-test n) (fast-prime? n (- times 1))]
        [else false]))

;;; 1.3 Formulating Abstractions with Higher-Order Procedures
;; 1.3.1 Procedures as Arguments
(define (cube x) (* x x x))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

; abstracting the three functions above
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; 1.3.2 Constructing Procedures Using lambda
; pi-sum without lambda
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; pi-sum with lambda
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

; integral without lambda
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; integral with lambda
(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

; Using Local Variables
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

; Using lambda
(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

; Using let
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

; Using define
(define (f x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))

;; 1.3.3 Procedures as General Methods
; half-interval search
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond [(positive? test-value)
                 (search f neg-point midpoint)]
                [(negative? test-value)
                 (search f midpoint pos-point)]
                [else midpoint])))))
(define (close-enough? x y) (< (abs (- x y)) 0.001))
(define (average a b) (/ (+ a b) 2.0))

; improved half-interval search
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond [(and (negative? a-value) (positive? b-value))
           (search f a b)]
          [(and (negative? b-value) (positive? a-value))
           (search f b a)]
          [else
           (error "Values are not of opposite sign" a b)])))

; finding fixed points
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; square root using fixed point(infinite loop)
(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

; square root using fixed point
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;; Procedures as Returned Values
; average damping
(define (average-damp f)
  (lambda (x) (average x (f x))))
; reformulated square-root
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
; cute-root by generalizing square-root
(define (cube-root x)
  (fixed-point (average-damp (lmabda (y) (/ x (square y))))
               1.0))
; derivative procedure
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)
; cube
(define (cube x) (* x x x))
;Newton's method as a fixed-point process
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
; square-root with Newton's method
(define (sqrt x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))