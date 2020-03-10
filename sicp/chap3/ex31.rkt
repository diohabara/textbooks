;; 3.31
; from chapter
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

; here are just syntax sugars
(define (get-signal wire) (wire 'get-signal))
(define (set-signal wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))
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

; Why this initialization of make-wire is necessary?(accept-action-procedure!)
; => when the procedure is executed, the delay time is added to the-agenda
; so it is necessary
; if accept-action-procedure! is define like below
(define (accept-action-procedure! proc)
  (set! action-procedures
        (cons proc action-procedures)))
; how the system's response would differ?
; => then the time will not be counted