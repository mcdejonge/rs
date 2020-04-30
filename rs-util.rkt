#lang racket

;; Utility functions for use in rs

(provide
 rs-util-rtsleep
 rs-util-diag
 rs-util-set-diag-mode
 rs-util-loop-procedure-and-wait
 )

;; Diagnosis mode. When turned on it prints diagnostic messages.
(define rs-util-diag-mode #t)
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



(define/contract (rs-util-rtsleep ms [pulse-length 100])
  ; Sleep for the given number of milliseconds. pulse-length is the
  ; number of milliseconds between checks against the time of the
  ; clock. The idea is that using smaller sleep increments will keep
  ; the timing more exact.
  ; TODO return the delta? Or leave this to someone else?
  (->* (positive?)
       (positive?)
       void)
  (let* ([now (truncate (current-inexact-milliseconds))]
         [end-time (+ now ms)]
         [delta (- end-time now)])
    (cond [(< delta 1) void]
          [(<= delta pulse-length)
           (sleep (/ delta 1000.0))]
          [else (sleep (/ pulse-length 1000.0))
                (rs-util-rtsleep (- end-time (truncate (current-inexact-milliseconds))) pulse-length)])
    ))

(define (rs-util-rtsleep-measure ms pulse-length)
  ;; Helper function for timinga rs-util-rtsleep 
  (-> positive? positive? void)
  (printf "~s sleeping for ~s should stop at ~s\n"
          (truncate (current-inexact-milliseconds))
          ms
          (+ (current-inexact-milliseconds) ms))
  (rs-util-rtsleep ms pulse-length)
  (printf "~s slept    for ~s should now  be ~s\n"
          (truncate (current-inexact-milliseconds))
          ms
          (current-inexact-milliseconds)))


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
    (rs-util-diag "Loop of ~s ms had to run at ~s but ran for ~s
\tUntil ~s (now: ~s) so difference is ~s
\tLoop must now run for ~s which means ~s\n"
                  correct-loop-length
                  prev-loop-length
                  (+ prev-loop-length difference)
                  expected-end
                  now
                  difference
                  next-loop-length
                  real-next-loop-length)
    ;; TODO if difference is < 5 try to bring the next-loop-length back to what it should be.
    (list real-next-loop-length next-loop-end)))
 
(define (rs-util-loop-procedure-and-wait procedure loop-length max-difference)
  ;; Loop the supplied procedure and wait for the given number of ms.
  ;; Attempts to correct the time to wait based on the duration of the
  ;; last iteration. The correction will not, however, be more than
  ;; max-difference * loop-length more or less than loop-length.
  ;;
  ;; The procedure MUST return true or false. If it returns false the
  ;; loop stops.
  (-> procedure? positive? positive?)
  (let loop ([next-loop-length loop-length]
             [expect-end-at (+ (current-inexact-milliseconds) loop-length)])
    (rs-util-diag "Starting new iteration of ~s loop with length ~s\n"
                  loop-length next-loop-length)
    (when (not (xor (procedure) (rs-util-rtsleep next-loop-length 2) ))
        (apply loop (rs-util-next-loop-length loop-length
                                              next-loop-length
                                              expect-end-at
                                              max-difference)))))

;; (define teller 0)
;; (rs-util-loop-procedure-and-wait
;;  (lambda () (
;;              (if (> teller 9)
;;                  (lambda () #f)
;;                  (and (printf "Procedure is called\n") (set! teller (+ teller 1)) (lambda () #t)))))
;;  300
;;  1/10)



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
