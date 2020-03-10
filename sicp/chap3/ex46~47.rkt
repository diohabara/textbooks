;; 3.46
; suppose that we implement test-and-set! using an ordinary procedure as shown
; in the text, without attempting to make the operation atomic.
; pic => skip

;; 3.47
; from text
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
; implement semaphore
; a: in terms of mutexes
(define (make-semaphore n)
  (let ((count 0) ; this is the max flags
        (the-mutex (make-mutex))) ; using mutex
    (define (the-semaphore m)
      (cond [(eq? m 'acquire)
             ; if message is 'release
             ; mutex will be 'acquire
             (the-mutex 'acquire)
             (if (= count n)
                 ; if the count is full
                 ; mutex will be released
                 ; the semaphore will be acquire
                 (begin
                   (the-mutex 'release)
                   (the-semaphore 'acquire))
                 ; otherwise
                 ; count will added by 1
                 ; the mutex be released
                 (begin
                   (set! count (+ count 1))
                   (the-mutex 'release)))]
            [(eq? m 'release)
             ; if message is 'release
             ; mutex will be 'acquire
             (the-mutex 'acquire)
             ; otherwise
             (if (zero? count)
                 ; if the count is empty
                 ; mutex will be release
                 (the-mutex 'release)
                 ; otherwise, count plus one and mutex will be release
                 (begin
                   (set! count (- count 1))
                   (the-mutex 'release)))]))
    the-semaphore))
      

; b: in terms of atomic test-and-set! operations
(define (make-semaphore n)
  (let ((count 0) 
        (cell (list #f)))
    (define (the-semaphore m)
      (cond [(eq? m 'acquire)
             (if (or (test-and-set! cell)
                     (>= count n))
                 (the-semaphore m) ; retry
                 (begin
                   (set! count (+ count 1))
                   (clear! cell)))
             [(eq? m 'release)
              (set! count (- count 1))
              (clear! cell)]]))
    the-semaphore))