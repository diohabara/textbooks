;; 3.33
; FROM TEXTBOOK

; adder
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond [(and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me)]
          [(and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me)]
          [(and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me)]))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond [(eq? request 'I-have-a-value) (process-new-value)]
          [(eq? request 'I-lost-my-value) (process-forget-value)]
          [else (errorr "Unknown request: ADDER" request)]))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

; multiplier
(define (mutiplier m1 m2 product)
  (define (process-new-value)
    (cond [(or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me)]
          [(and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me)]
          [(and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me)]
          [(and (has-value? product) (has-value? m2))
           (set-value m1
                      (/ (get-value product)
                         (get-value m2))
                      me)]))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond [(eq? request 'I-have-a-value) (process-new-value)]
          [(eq? request 'I-lost-a-value) (process-forget-value)]
          [else (error "Unknown request: MULTIPLIER"
                       request)]))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

; constant
; connect the value to the connector and set the value
(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

; define a procedure averager, using adder, multiplier, and constant constraints
; averager: takes three connectors a, b, and c as inputs
; and the c is the average of the values of a and b

(define (averager a b c)
  (let ((u (make-connector))
        (x (make-connector)))
    (adder a b u)
    (multiplier u x c)
    (constant 2 x)
    'ok))

;; 3.34
; Louis Reasoner's squarer
(define (squarer a b)
  (multiplier a a b))
; but this has a flaw.
; multiplier returns a value if two of the three arguments' values are determined
; but in this procedure, even if the b is determined, both of a may not be determined
; this may cause a serious problem

;; 3.35
; as a new primitive constraint
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER"
                   (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
        (set-value! b
                    ( * (get-value a) (get-value a))
                    me)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me))
  (define (me request)
    (cond [(eq? request 'I-have-a-value) (process-new-value)]
          [(eq? request 'I-lost-a-value) (process-forget-value)]
          [else (error "Unknown request: SQUARER")
                request]))
  (connect a me)
  (connect b me)
  me)

;; 3.36
; suppose we evaluate the following sequence of expression in the global environment
(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)

; at some time during evaluation of the set-value!,
; the following expression from the connector's local procedure is evaluated
(for-each-except
 setter inform-about-value constraints)
; diagram problem => skip :>

; 3.37
; more expression-oriented style of definition of celsius-fahrenheit converter
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

; implement constraint versions of arithmetic operations
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))
(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))
(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))