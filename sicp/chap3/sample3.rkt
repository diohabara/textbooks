;;;; chap3 Modularity, Objects, and State
;;; 3.1 Assignment and Local State
;; 3.1.1 Local State Variables
; initial balance
(define balance 100)
; withdraw amount from balance
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
; (set! <name> <new-value>)
; (begin <exp1> <exp2> ... <expk>): will evaluete respectively from 1 to k

; make balance internal to withdraw
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

; make-withdraw specifies the initial amount of money
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

; here also define deposit
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "Unknown request: MAKE-ACCOUNT"
                       m)]))
  dispatch)

;; 3.1.2 the Benefits of Introducing Assignment
; rand: returns random number, with a random-init, and change x by rand-update
(define random-init 12345)
(define (rand-update x)
  (modulo (+ (* 214013 x) 253011) 32767))
(define rand (let ((x random-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

; estimate-pi: return pi using the result of monte-carlo
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
; cesaro-test: return if two random numbers' gcd is 1
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
; monte-carlo: return 6/pi^2
; the result is the probablity of chosen two integers that have factors in common
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond [(= trials-remaining 0)
           (/ trials-passed trials)]
          [(experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1))]
          [else
           (iter (- trials-remaining 1)
                 trials-passed)]))
  (iter trials 0))

; estimate-pi: return pi
(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))
; return: return 6/pi^2
; the same as the previous one, but here using rand-update
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond [(= trials-remaining 0)
               (/ trials-passed trials)]
              [(= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2)]
               [else
                (iter (- trials-remaining 1)
                      trials-passed
                      x2)]))))
  (iter trials 0 initial-x))

;; 3.1.3 The Costs of Introducing Assignment
; make-simplified-withdraw: simplified so that it does not bother to check for an insufficient amount
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

; make-decrementer: does not use set!
; there is no accumulated effect over successive calls
(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

; factorial: iteratively return factorial number
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))))
  (iter 1 1))
; factorial; more imperative style by using explicit assignment
(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))

;;; 3.2 The Environment Model of Evaluation
;; 3.2.1 The Rules for Evaluation
; evaluate in the global environment
(define (square x)
  (* x x))
; equivalent to this lambda expression
(define square
  (lambda (x) (* x x)))

;; 3.2.2 Applying Simple Procedures
(define (square x)
  (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;; 3.2.3 Frames as the Repository of Local State
; here is an example of how procedures and assignment can be used
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

;; 3.2.4 Internal Definitions
; sqrt: compute square root
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(define (average a b)
  (/ (+ a b) 2))


;;; 3.3 Modeling with Mutable Data
;; 3.3.1 Mutable List Structure
; cons: using set-car! and set-cdr!
;(define (cons x y)
;  (let ((new (get-new-pair)))
;    (set-car! new x)
;    (set-cdr! new y)
;    new))

;; Sharing and identity
(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
(define (set-to-wow! x) (set-car! (car x) 'wow) x)
; (eq? (car z1) (cdr z1))
; => true
; (eq? (car z2) (cdr z2))
; => false

; Mutation is just assignment
; from section 2.1.3
(define (cons x y)
  (define (dispatch m)
    (cond [(eq? m 'car) x]
          [(eq? m 'cdr) y]
          [else (error "Undefined operation: CONS" m)]))
  dispatch)
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

; mutable data objects as procedures using assignment and local state
(define (cons x y)
  (define (set-x! v) (set! x y))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond [(eq? m 'car) x]
          [(eq? m 'cdr) y]
          [(eq? m 'set-car!) set-x!]
          [(eq? m 'set-cdr!) set-y!]
          [else
           (error "Undefined operation: CONS" m)]))
  dispatch)
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value) z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value) z)

;; 3.3.2 Representing Queues
; queue: FIFO(first in, first out) data structure
(define (front-ptr queue) (car queue)) ; return the front pointer of queue
(define (rear-ptr queue) (cdr queue)) ; return the rear pointer of queue
(define (set-front-ptr! queue item) ; set item in the front of queue
  (set-car! queue item))
(define (set-rear-ptr! queue item) ; set item in the rear of queue
  (set-cdr! queue item))
(define (empty-queue? queue) ; check if the queue is empty
  (null? (front-ptr queue)))
(define (make-queue) (cons '() '())) ; make a new queue

 ; return the front of queue
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

; insert a new item in the rear of the queue
(define (insert-queue! queue item)
  (let ((new-pair (cons item '()))) ; new-pair is the item and the end point
    (cond [(empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue]
          [else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue])))

; delete the front item of the queue
(define (delete-queue! queue)
  (cond [(empty-queue? queue)
         (error "DELETE! called with an empty queue" queue)]
        [else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue]))

;; 3.3.3 Representing Tables
; if the returned record given by assoc is not false, return the cdr of the record
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

; return a record with the given key
(define (assoc key records)
  (cond [(null? records) #f]
        [(equal? key (caar records)) (car records)]
        [else (assoc key (cdr records))]))

; first see if there is already a record in the table using assoc
; if not, form a new record which cons key and value nad insert it at the head
; otherwise, set cdr of record to the new value
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table)))))
  'ok)

; construct a new table
(define (make-table)
  (list '*table*))

;; Two-dimentional tables
; first search key-1 in the table
; if any, search key-2 in the subtable, then return the answer
(define (lookup key-1 key-2 table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

; see if there is a subtable stored undder the first key
; if not, build a new subtable containing the single record(key-2, value)
; and insert it into the table under the first key
; if there is, insert the new record into this subtable using the insertion method
(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (ser-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

;; Creating local tables
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) #f))
            #f)))
  (define (insert! key-1 key-2 value)
    (let ((subtable
           (assoc key-1 (cdr local-table))))
      (if subtable
          (let ((record
                 (assoc key-2 (cdr subtable))))
            (if record
                (set-cdr! record value)
                (set-cdr! subtable
                          (cons (cons key-2 value)
                                (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1 (cons key-2 value))
                          (cdr local-table)))))
    'ok)
    (define (dispatch m)
      (cond [(eq? m 'lookup-proc) lookup]
            [(eq? m 'insert-proc!) insert!]
            [else (error "Unknown operation: TABLE" m)]))
    dispatch))

; implementation of put and get in the chap2
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; 3.3.4 A Simulator for Digital Circuits

; more simple way
(define (half-addder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inventer c e)
    (and-gate d e s)
    'ok))

; construct full-adder
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; Primitive function boxes
; (get-signal <wire>) => the current value on the wire
; (set-signal! <wire> <new value>) => change the current value on the wire
; (add-action! <wire> <procedure of no arguments>) => asserts the procedure should be run on the wire
; (after-dealy <time> <procedure>) => execute the procedure after the delay

; intverter
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)
; logical-not
(define (logical-not s)
  (cond [(= s 0) 1]
        [(= s 1) 0]
        [else (error "Invalid signal" s)]))

; and-gate
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-dealy
       and-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;; Representing wires
; make a wire
(define (make-wire)
  (let ((signal-value 0) (action-procedurs '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond [(eq? m 'get-signal) signal-value]
            [(eq? m 'set-signal!) set-my-signal!]
            [(eq? m 'add-action!) accept-action-procedure!]
            [else (error "Unknown operation: WIRE" m)]))
    dispatch))
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each cdr procedures))))
(define (get-signal wire) (wire 'get-signal))
(define (set-signal wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;; The agenda
; (make-agenda) => a new empty agenda
; (empty-agenda? <agenda>)
; (first-agenda-item <agenda>) => the first item on the agenda
; (remove-first-agenda-item! <agenda>) => remove the first item on teh agenda
; (add-to-agenda! <time> <action> <agenda>) => add the given action to be run a the specified time
; (current-time <agenda>) => current simulation time

; plus delay of the action onto the agenda
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

; execute all the items on the agenda
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;; A sample simulation
; probe: return the value that has changed since the actino is done,
; current time and a name that identifies the wire
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

; initialization of the agenda
(define (make-agenda) (list 0))
(define the-agenda make-agenda)
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; define four wires
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

;; Implementing the agenda
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

; agenda itself is a one-dimentional table of time segments
(define (make-agenda) (list 0)) ; agenda is headed by the time
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segment agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  ; see if segments is empty or bigger time
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  ; make a new segment with segment and time
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    ; if the segment-time is the same as the one of segments, add it
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        ; otherwise
        ; make a cdr of segments concatenation of (time action) and segments
        ; do it until ihis procedure is done
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  ; execution
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

; delete the first item from the agenda
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))


(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;; 3.3.5 Propagation of Constraints

;; Using the constraint system
(define C (make-connector))
(define F (make-connector))
(define (celsius-fahrenheit-converter c f)
  ; internal connector: u, v, w, x, y
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

; print a message when the connector is given a value
(probe "Celsius tmp" C)
(probe "Fahrenheit temp" F)

; (has-value? <connector>); whether the connector has a value
; (get-value <connector>); the connector's current value
; (set-value! <connector> <new-value> <informant>)
; => the informant is requesting the connector to set its value to the new value
; (forget-value! <connector> <retractor>)
; => the retractor is requesting it to forget its value
; (connect <connector> <new-constraint>)
; => the connector participate in the new constraint

; inform-about-value: the connector has a value
; inform-about-no-value: the connector has lost its value

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

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ") (display name)
    (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond [(eq? request 'I-have-a-value) (process-new-value)]
          [(eq? request 'I-lost-my-value) (process-fortget-value)]
          [else (error "Unknown request: PROBE" request)]))
  (connect connector me)
  me)

;; Representing connectors
; value: local statee variable
; informant: the current value of the connector
; constraints: a list of the constraints in which the connector participates
(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-my-value newval setter)
      (cond [(not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints)]
            [(not (= value newval))
             (error "Contradictoin" (list value newval))]
            [else 'ignored]))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint contrainsts))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond [(eq? request 'has-value?)
             (if informant #t #f)]
            [(eq? request 'value) value]
            [(eq? request 'set-value!) set-my-value]
            [(eq? request 'forget) forget-my-value]
            [(eq? request 'connect) connect]
            [else (error "Unknown operation: CONNECTOR"
                         request)]))
    me))
; follow the procedure to the all the items in the list except exception
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond [(null? items) 'done]
          [(eq? (car items) exception) (loop (cdr items))]
          [else (procedure (car items))
                (loop (cdr items))]))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connecto 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;;; 3.4 Concurrency: Time Is of the Essence
;; 3.4.1 The Nature of Time in Concurrent Systems
; as a seperate process of withdrawal
(define (withdraw amount)
  (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))

;; Correct behavior of concurrent programs

;; 3.4.2 Mechanisms for Controlling Concurrency
;; Serializing access to shared state
;; Serializers in Scheme
; (parallel-execute <p1> <p2> .. <pk>)
; each <p> must be a procedure of no arguments.

; this can cause interleaving
(define x 10)
(parrallel-execute
 (lambda () (set! x (* x x)))
 (lambda () (set! x (+ x 1))))

; using serialize, cannot cause interleaving
(define x 10)
(define s (make-serializer))
(parallel-execute
 (s (lambda () (set! x (* x x))))
 (s (lambda () (set! x (+ x 1)))))

; from 3.1.1
(define (make-account balance)
  ; withdraw money from bank account
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  ; deposit money into bank account
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protecteed (make-serializer)))
    (define (dispatch m)
      (cond [(eq? m 'withdraw) (protected withdraw)]
            [(eq? m 'deposit) (protected deposit)]
            [(eq? m 'balance) balance]
            [else (error "Unknown request: MAKE-ACCOUNT"
                         m)]))
    dispatch))

;; Complexity of using multiple shared resources
; this procedure may cause a problem, when exchanging a1 and a2, a1 and a3 concurrently
(define (exchange account1 account2)
  (let ((diffenrence (- (account1 'balance)
                        (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

; to assert concurrent access to accounts
; arrange for access to an account's serializer
; this is almost identical to the one in 3.1.1
; here the responsibility is imposed on each account
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond [(eq? m 'withdraw) withdraw]
            [(eq? m 'deposit) deposit]
            [(eq? m 'balance) balance]
            [(eq? m 'serializer) balance-serializer]
            [else (error "Unknown request: MAKE-ACCOUNT" m)]))
    dispatch))
; this is the example
(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

; exporting the serialize gives us enough flexibility
(define (serialized-exchange account1 account 2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

;; Implementing serailizers
(define (make-serailizer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond [(eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))] ; retry
            [(eq? m 'release) (clear! cell)]))
    the-mutex))
(define (clear! cell) (set-car! cell #f))
(define (test-and-set! cell)
  (if (car cell) #t (begin (set-car! cell #t) #f)))

;;; 3.5 Streams
;; 3.5.1 Streams Are Delayed Lists
; prime? from a previous section
(define (filter lst p)
    (cond ((null? lst)   (list))
          ((p (car lst)) (cons (car lst) (filter (cdr lst) p)))
          (else          (filter (cdr lst) p))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
; compute th sum of all the prime numbers
; 1st
(define (sum-primes a b)
  (define (iter count accum)
    (cond [(> count b) accum]
          [(prime? count)
           (iter (+ count 1) (+count accum))]
          [else (iter (+ count 1) accum)]))
  (iter a 0))

; 2nd
(define (sum-primes a b)
  (accumulate +
              0
              (filter prime?
                      (enumerate-interval a b))))
; cons-stream: constructor
; stream-car: selector
; stream-cdr: selector

; procedures in order to use aggregate data
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (straem-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

; (cons-stream <a> <b>)
; = (cons <a> (delay <b>))
(define (cons-stream a b)
  (cons a (delay b)))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;; The stream implementation in action
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))


; the result returned by this, formed by the cons-stream
(cons 10000
      (delay (stream-enumerate-interval 1001 1000000)))

; stream-filter analog of the filter
(define (stream-filter pred stream)
  (cond [(stream-null? stream) the-empty-stream]
        [(pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream)))]
        [else (stream-filter pred (stream-cdr stream))]))

;; Implementing delay and force
; ordinary delay
(define (delay exp)
  (lambda () exp))
; ordinary force
(define (force delayed-object)
  (delayed-object))
; memo for delayed evaluation
(define (memo-proc proc)
  (let ((already-run? #f)
        (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))
; better delay
(define (delay exp)
  (memo-proc (lambda () exp)))

;; 3.5.2 Infinite Stream
; infinite sequences can be represented by stream
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

; using integer, here we defined number that is not divisible by 7
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))


; sieve of eratosthenes
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

;; Defining streams implicitly
(define ones (cons-stream 1 ones))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integrs-starting-from 3))))
(define (prime? n)
  (define (square x) (* x x))
  (define (iter ps)
    (cond [(> (square (strea-car ps)) n) #t]
          [(divisible? n (stream-car ps)) #f]
          [else (iter (stream-cdr ps))]))
  (iter primes))

;; 3.5.3 Exploiting the Stream Paradigm
;; Formulating iterations as stream processes
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

; accelerated sequence
(define (euler-transform s)
  (let ((s0 (stream-ref s 0)) ; S_n-1
        (s1 (stream-ref s 1)) ; S_n
        (s2 (stream-ref s 2))) ; S_n+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

; even more accelerated sequence, (but this version is multi-dimentional)
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

; take the first row of it
(define (accelerated-sequence tranform s)
  (stream-map stream-car (make-tableau transform s)))

;; Infinite strams of pairs
; this is the pair of (i, j) s.t., i+j is prime
(stream-filter
 (lambda (pair) (prime? (+ (car pair) (cadr pair))))
 int-pairs)

; in the (pairs S T), the second piece(the rest of the first row) is
(stream-map (lambda (x) (list (stream-car s) x))
            (stream-cdr t))

; we can form out stream of pairs as follows
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   ((interleave s t)
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

; combine 2 streams
; but this method needs finite sequence 
(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

; elegant way to reach every element eventually
; since interleave takes elements alternately find its way into the interleaved stream
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;; Stream as signals
; integrator of signals
(define (integral integrand initial-value dt)
  (define int
    (cons-stram initial-value
                (add-streams (scale-stream integrand dt)
                             int)))
  int)

;; 3.5.4 Streams and Delayed Evaluation
; procedure int 
(define int
  (cons-stream
   initial-value
   (add-streams (scale-stream integrand dt)
                int)))

; this procedure does not work
; because solve calls integral that requires input dy which is not define until the third line
(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)

; redefinition of integral
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt) int))))
  int)

; now we can implement our solve procedure by delaying the evaluation of dy
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; Normal-order evaluation

;; 3.5.5 Modularity of Functional Programs and Modularity of Objects
; generator of random numbers
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define random-numbers
  (cons-stream
   random-init
   (stream-map rand-update random-numbers)))

(define cesaro-stream
  (map-successive-pairs
   (lambda (r1 r2) (= (gcd r1 r2) 1))
   random-numbers))
(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

; monte carlo
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-straem
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (netx passed (+ failed 1))))
; pi
(define pi
  (stream-map
   (lambda (p) (sqrt (/ 6 p)))
   (monte-carlo cesaro-stream 0 0)))

;; A fuctional-programming view of time
; assignment
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

; stream
(define (stream-withdraw balance amount-stream)
  (cons-stream
   balance
   (stream-withdraw (- balance (stream-car amount))
                    (stream-cdr amount-stream))))