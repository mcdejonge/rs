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
 rs-util-calc-time-corrected
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

(define (rs-util-run-timed-ms proc)
  ;; Run a procedure and return the time it took.
  (define now (current-inexact-milliseconds))
  (proc)
  (- (current-inexact-milliseconds) now))

(define/contract (rs-util-calc-time-corrected pref-length last-diff [max-diff-ratio 1/20])
  (->* (positive? number?)
       (positive?) positive?)
  ;; Calculate the corrected length of something (a step or a loop)
  ;; taking into account the last time it ran. max-diff-ratio is the
  ;; limit to how much correction will take place (as a ratio of the
  ;; preferred length).
  (define max-diff (* pref-length max-diff-ratio))
  (define min-length (- pref-length max-diff))
  (define max-length (+ pref-length max-diff))
  (max min-length (min (- pref-length last-diff) max-length)))


(define/contract (rs-util-loop-and-wait procedure loop-length [max-difference 1/10])
  ;; Loop the supplied procedure for the given number of ms.
  ;; Attempts to correct the time to wait based on the duration of the
  ;; last iteration. The correction will not, however, be more than
  ;; max-difference * loop-length more or less than loop-length.
  ;;
  ;; If supplied a procedure this procedure MUST return true or
  ;; false. If it returns false the loop stops.
  (->* (procedure? positive?)
       (positive?)
       any)
  (let loop ([last-diff 0])
    (define corrected-loop-length (rs-util-calc-time-corrected loop-length last-diff))

    (rs-util-diag "Starting new iteration of a loop with proper length ~s that will have length ~s\n"
                  loop-length corrected-loop-length)
    (define start-time (current-inexact-milliseconds))
    (define result (procedure))
    (rs-util-rtsleep corrected-loop-length)
    (when result
           (loop (- (- (current-inexact-milliseconds) start-time) corrected-loop-length)))))
  
