;; 3.28
; implement or-gate
(define (or-gate in1 in2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal in1) (get-signal in2))))
      (after-dealy
       or-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! in1 or-action-procedure)
  (add-action! in2 or-action-procedure)
  'ok)

;; 3.29
; implement or-gate in a diffrent way from the one above
; using and-gates and inverters
; think about the delay time in terms of and-gate-delay and inverter-delay

; inverter
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
; or-gate
; a or b
; = (not (not a)) or (not (not b))
; = (not ((not a) and (not b)))
(define (or-gate in1 in2 output)
  (let ((a (make-wire))
        (b (make-wire))
        (c (make-wire)))
    (inverter in1 a)
    (inverter in2 b)
    (and-gate a b c)
    (inverter c output)
    'ok))

; delay time is 2*inverter-delay  + and-gate-delay

;; 3.30
; half-adder
(define (half-addder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inventer c e)
    (and-gate d e s)
    'ok))nnn

; full-adder
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; ripple-carry-adder 
(define (ripple-carry-adder list-a list-b list-sum c-out)
  (define (iter list-a list-b list-sum c-in)
    (if (not (null? list-a))
        (let ((c-out (make-wire)))
          (full-adder (car list-a) (car list-b) c-in (car list-sum) c-out)
          (iter (cdr list-a) (cdr list-b) (cdr list-sum) s-out))
        'ok))
  (iter list-a list-b list-sum c-out))

