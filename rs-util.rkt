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
  (->* (natural?)
       (natural?)
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
  (-> natural? natural? void)
  (printf "~s sleeping for ~s should stop at ~s\n"
          (truncate (current-inexact-milliseconds))
          ms
          (+ (current-inexact-milliseconds) ms))
  (rs-util-rtsleep ms pulse-length)
  (printf "~s slept    for ~s should now  be ~s\n"
          (truncate (current-inexact-milliseconds))
          ms
          (current-inexact-milliseconds)))




  ;; (let* ([end-time (+ ms (current-inexact-milliseconds))])
  ;;   (printf "Sleeping for ~s ms should stop at ~s\n" ms end-time)
  ;;   (for/fold ([next-pulse (+ pulse-length (current-inexact-milliseconds))])
  ;;             ([i (in-range (quotient ms pulse-length))])
  ;;     (printf "Iteration ~s should stop at ~s\n" i next-pulse)
  ;;     (sleep (/ pulse-length 1000.0))
  ;;     (+ next-pulse pulse-length))
  ;;   (when (< (inexact->exact (truncate (current-inexact-milliseconds)))
  ;;            (inexact->exact (truncate end-time)))
  ;;     (printf "Must sleep for remainder\n")
  ;;     (sleep (/ (- end-time (current-inexact-milliseconds)) 1000.0)))
  ;;   (printf "Done sleeping at ~a\n" (current-inexact-milliseconds))))



  

