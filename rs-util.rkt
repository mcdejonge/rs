#lang racket/base

(require racket/bool
         racket/contract/base
         racket/contract/region)



;; Utility functions for use in rs

(provide
 rs-util-rtsleep
 rs-util-diag
 rs-util-set-diag-mode
 rs-util-loop-and-wait
 rs-util-run-timed-ms
 )


;; Diagnosis mode. When turned on it prints diagnostic messages.
(define rs-util-diag-mode #f)
(define (rs-util-set-diag-mode true-or-false)
  (set! rs-util-diag-mode true-or-false))

(define (rs-util-diag message . args)
  ;; Print a diagnostic message (using printf) but only if
  ;; rs-util-diag-mode is #t.
  ;;
  ;; NOTE: if you need to perform a function call in one of your args,
  ;; make sure it only happens when diag-mode is #t, in other words
  ;; supply a procedure object rather than the result of a procedure
  ;; call. If you do not do this, performance will suffer greatly as
  ;; your procedure calls will also be executed if they don't need to
  ;; be (namely when diag-mode is #f).
  (when rs-util-diag-mode
    (apply printf (cons message
                        (map (lambda (item)
                               (cond [(procedure? item) (item)]
                                     [else item]))
                             args)))))



(define/contract (rs-util-rtsleep ms)
  ;; Sleep for the given number of milliseconds. More accurate than
  ;; (sleep) because it checks every millisecond if it should wake up.
  (-> positive? void)
  (let ([end (+ (current-inexact-milliseconds) ms)])
    (let loop ()
      (when (< (current-inexact-milliseconds) end)
        (sleep 0.001)
        (loop)))))


(define (rs-util-next-loop-length correct-loop-length prev-loop-length expected-end max-difference)
  ;; Calculate the length of a loop taking into account how long it
  ;; took the last one to complete vs how long we expected it to take.
  ;; The difference Take into account the correct loop length. The difference between
  ;; the new loop length and the correct loop length will no more than
  ;; max-difference * the correct loop length.
  ;;
  ;; Returns a list containing the a new loop length and a new end time.
  (rs-util-next-loop-length-now (current-inexact-milliseconds) correct-loop-length prev-loop-length expected-end max-difference))

(define 
  (rs-util-next-loop-length-now now correct-loop-length prev-loop-length expected-end max-difference)
  ;; This is the real calculation function. It's separated out so it can be tested.
  (let* ([difference (- now expected-end)]
         [min-loop-length (- correct-loop-length (* max-difference correct-loop-length))]
         [max-loop-length (+ correct-loop-length (* max-difference correct-loop-length))]
         [next-loop-length (- correct-loop-length difference)]
         [real-next-loop-length
          (max min-loop-length (min next-loop-length max-loop-length))]
         [next-loop-end (+ now next-loop-length)])
    (rs-util-diag "Loop of ~s ms had to run at ~s but ran for ~s. Next iter will be ~s\n"
                  correct-loop-length
                  prev-loop-length
                  (+ prev-loop-length difference)
                  real-next-loop-length)
    ;; TODO if difference is < 5 try to bring the next-loop-length back to what it should be.
    (list real-next-loop-length next-loop-end)))

(define (list-or-procedure? input)
  (or (list? input) (procedure? input)))

(define (rs-util-run-timed-ms proc)
  ;; Run a procedure and return the time it took.
  (define now (current-inexact-milliseconds))
  (proc)
  (- (current-inexact-milliseconds) now))

(define (rs-util-loop-and-wait list-or-procedure loop-length max-difference)
  ;; Loop the supplied procedure or list of procedures and wait for the given number of ms.
  ;; Attempts to correct the time to wait based on the duration of the
  ;; last iteration. The correction will not, however, be more than
  ;; max-difference * loop-length more or less than loop-length.
  ;;
  ;; If supplied a procedure this procedure MUST return true or
  ;; false. If it returns false the loop stops.
  ;;
  ;; If supplied a list it will loop through each item. If an item is
  ;; a procedure it will call it.
  (-> list-or-procedure? positive? positive?)
  (when (procedure? list-or-procedure)
    (let loop ([next-loop-length loop-length]
               [expect-end-at (+ (current-inexact-milliseconds) loop-length)])
      (rs-util-diag "Starting new iteration of a loop with proper length ~s that will have length ~s\n"
                    loop-length next-loop-length)
      (when (not (xor (list-or-procedure) (rs-util-rtsleep next-loop-length ) ))
        (apply loop (rs-util-next-loop-length loop-length
                                              next-loop-length
                                              expect-end-at
                                              max-difference)))))
  ;; We're dealing with a list. Iterate through it and call items as needed.
  (when (list? list-or-procedure)
    (for/fold ([next-loop-length loop-length]
               [expect-end-at (+ (current-inexact-milliseconds) loop-length)])
              ([item list-or-procedure])
      (when (procedure? item)
        (item))
      (rs-util-rtsleep next-loop-length)
      (apply values (rs-util-next-loop-length loop-length
                                              next-loop-length
                                              expect-end-at
                                              max-difference))
              )
    
    )
  )

(module+ test
  (require rackunit)

  ;; Function for running simple tests in which the new loop length is
  ;; within the acceptable difference from the correct loop length.
  (define (test-function-within-allowed-difference
           correct-loop-length prev-loop-length real-loop-length expected-new-loop-length)
    (let* ([now 150000]
           [prev-loop-length 300]
           [expected-end (+ (- now real-loop-length) prev-loop-length)]
           [max-difference 1/10]
           [result (rs-util-next-loop-length-now now
                                                 correct-loop-length
                                                 prev-loop-length
                                                 expected-end
                                                 max-difference)])
      (check-equal? result (list expected-new-loop-length (+ now expected-new-loop-length)))
      )
    )
  ;; Real result is too long.
  (test-function-within-allowed-difference 300 300 303 297)
  ;; Real result is too short.
  (test-function-within-allowed-difference 300 300 297 303)
  
  )
