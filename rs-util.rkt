#lang racket

;; Utility functions for use in rs

(provide
 rs-util-rtsleep
 rs-util-diag
 rs-util-set-diag-mode
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
  ;; supply a procedure object rather than the result of the procedure
  ;; call. If you do not do this, performance will suffer greatly as
  ;; your procedure calls will also be executed if they don't need to
  ;; be (namely when diag-mode is #f).
  (when rs-util-diag-mode
    (apply printf (cons message
                        (map (lambda (item)
                               (cond [(procedure? item) (item)]
                                     [else item]))
                             args)))))

(rs-util-diag "Koos ~s\n" (lambda () (printf "Mag niet\n")))


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
  ;; Helper function for timinga rs-util-rtsleep 
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

