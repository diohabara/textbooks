; from the section
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

;; 3.21
; print-queue: print the sequence of items in the queue
(define (print-queue queue)
  (car queue))
(define q1 (make-queue))
(print-queue q1)
(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)

;; 3.22
; queue as procedure with local state
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    ;definitions of internal procedures
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? queue))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))
    (define (rear-queue)
      (if (empty-queue?)
          (error "REAR called with an empty queue")
          (car rear-ptr)))
    (define (insert-queue item)
      (let ((new-pair (cons item '())))
        (cond [(empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               front-ptr]
              [else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               front-ptr])))
    (define (delete-queue!)
      (cond [(empty-queue?)
             (error "DELETE! call with an empty queue")]
            [else 
             (set-front-ptr! (cdr front-ptr))]))
    (define (print-queue)
      front-ptr)
    (define (dispatch m)
      (cond [(eq? m 'insert-queue!) insert-queue!]
            [(eq? m 'delete-queue!) delete-queue!]
            [(eq? m 'front!) front-queue!]
            [(eq? m 'rear!) rear-queue!]
            [(eq? m 'print) print-queue!]
            [else
             (error "Undefined operation -- MAKE-QUEUE" m)]))
    dispatch))

;; 3.23
; impletation of deque, which is accesible from the front and the rear
(define (front-ptr deque) (car deque)) ; return fornt pointer of deque
(define (rear-ptr deque) (cdr deque)) ; return rear pointer of deque
(define (set-front-ptr! deque item) (set-car! deque item)) ; set item in the front pointer 
(define (set-rear-ptr! deque item) (set-cdr! deque item)) ; set item in the rear pointer
(define (make-item value) ; make a list (value (nil nil)
  (cons value (cons '() '())))
(define (set-next-item! item next) ; set rear element as item
  (set-cdr! (cdr item) next))
(define (set-prev-item! item prev) ; set front element as item
  (set-car! (cdr item) prev))
(define (next-item item) ; return rear element
  (cddr item))
(define (prev-item item) ; return front element
  (cadr item))
(define (value-of-item item) ; return front pointer
  (car item))
(define (empty-deque? deque) (null? (front-ptr deque))) ; check if it is empty
(define (make-deque) (list '() '())) ; make empty list
(define (front-deque deque) ; return front of deque
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      ((value-of-item (front-ptr deque)))))
(define (rear-deque deque) ; return rear of deque
  (if (empty-deque? deque)
      (errro "REAR called with an empty deque" deque)
      ((value-of-item (rear-ptr deque)))))
(define (front-insert-deque! deque value) ; insert a value at front 
  (let ((new-item (make-item value)))
    (cond [(empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)]
          [else
           (set-next-item! new-item (front-ptr deque))
           (set-front-ptr! deque new-item)
           deque])))
(define (rear-insert-deque! deque value) ; insert a value at rear
  (let ((new-item (make-item value)))
    (cond [(empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)
           deque]
          [else
           (set-prev-item! new-item (rear-ptr deque))
           (set-next-item! (rear-ptr deque) new-item)
           (set-rear-ptr! deque new-item)
           deque])))
(define (front-delte-deque! deque) ; delete front 
  (cond [(empty-deque? deque)
         (error "DELETE! called with an empty deque" deque)]
        [else
         (set-front-ptr deque (next-item (front-ptr deque)))
         deque]))
(define (rear-delete-deque! deque) ; delete rear
  (cond [(empty-deque? deque)
         (error "DELETE! called with an empty deque" deque)]
        [else
         (set-rear-ptr! deque (prev-item (rear-ptr deque)))
         deque]))
(define (print-deque deque) ; print deque
  (define (print-iter q)
    (cond [(eq? q (rear-ptr deque))
           (display " ")
           (display (value-of-item q))]
          [else
           (begin (display " ")
                  (display (value-of-item q))
                  (print-iter (next-item q)))]))
  (if (empty-deque? deque)
      (begin (display "empty\n"))
      (begin (display "(")
             (print-iter (front-ptr deque))
             (display ")"))))

; for test
(display "3.23\n")
(define q (make-deque))
(front-insert-deque! q 'a)
(print-deque q) ; ( a)
(front-insert-deque! q 'b)
(print-deque q) ; ( b a)
(rear-insert-deque! q 'c)
(print-deque q) ; ( b a c)
(rear-delete-deque! q)
(print-deque q) ; ( b a)