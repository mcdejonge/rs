#lang racket

;; Utility functions for use in rs

(provide
 rs-util-rtsleep
 rs-util-rtsleep-measure
 )

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
  ;; Take into account the correct loop length. The difference between the new loop length and the correct loop length will no more than max-difference * the correct loop length.
  ;; than 110% of the correct loop length.  Returns a list containing
  ;; the correct loop length, a new loop length and a new end time and
  ;; the difference.
  (let* ([now (truncate (current-inexact-milliseconds))]
         [difference (- now expected-end)]
         [min-loop-length (* max-difference correct-loop-length)]
         [max-loop-length (* max-difference correct-loop-length)]
         [next-loop-length (- prev-loop-length difference)]
         [real-next-loop-length
          (max min-loop-length (min next-loop-length max-loop-length))]
         [next-loop-end (+ now next-loop-length)])
    (list real-next-loop-length next-loop-end difference)))
 
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
             [expect-end-at (+ (current-inexact-milliseconds) loop-length)]
             [difference 0])
    (if (and (rs-util-rtsleep next-loop-length) (procedure))
        (apply loop (rs-util-next-loop-length loop-length
                                              next-loop-length
                                              expect-end-at
                                              max-difference))
        (void))))

(define teller 0)
(rs-util-loop-procedure-and-wait
 (lambda () (
             (if (> teller 10)
                 (lambda () #f)
                 (and (printf "Procedure is called\n") (set! teller (+ teller 1)) (lambda () #t)))))
 300
 1/10)
