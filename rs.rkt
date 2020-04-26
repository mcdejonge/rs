#lang racket

;; Main rs file. Contains the main loop and functions for interacting with it.

(provide
 rs-set-global-bpm!
 rs-set-global-div-length!
 rs-set-global-num-divs!
 rs-start-main-loop!
 rs-stop-main-loop!)

(require "rs-util.rkt")

(define rs-main-loop '())

(define rs-main-bpm 120)

(define rs-main-num-divs 16)

(define rs-main-div-length 1/4)

(define rs-tracks '())

; Helper timevalues for use in sleep

(define rs-main-div-length-ms 0.0)

(define rs-main-div-time-in-secs 0.0)

(define rs-main-sleep-time-in-secs 0.0) ; Not sure if this is needed?

(define (rs-main-recalculate-time-values!)
  (set! rs-main-div-length-ms
        (rs-calculate-div-length-ms rs-main-bpm rs-main-div-length))
  (set! rs-main-div-time-in-secs
        (/ rs-main-div-length-ms 1000))
  (set! rs-main-sleep-time-in-secs
    (* rs-main-num-divs rs-main-div-time-in-secs)))

;; TODO contracts may seem nice, but they cause the program to stop,
;; which is not what you want in a sequencer. For public functions
;; just output an error message, refuse to do something stupid and
;; continue.

(define/contract (rs-set-global-num-divs! num-divs)
  (-> positive? void)
  ; Set the global number of divs.
  (set! rs-main-num-divs num-divs)
  (rs-main-recalculate-time-values!))

(define/contract (rs-set-global-bpm! bpm)
  (-> positive? void)
  ; Set the global BPM.
  (set! rs-main-bpm bpm)
  (rs-main-recalculate-time-values!))

(define/contract (rs-set-global-div-length! div-length)
  (-> positive? void)
  (set! rs-main-div-length div-length)
  (rs-main-recalculate-time-values!))


; Void
(define (rs-start-main-loop!)
  ; Starts the main loop.
  (rs-main-recalculate-time-values!)
  (set! rs-main-loop
        (thread
         (lambda ()
           (let loop ()
             ; Below is an example of a loop with subdivs. The
             ; main loop will not do this, but the individual tracks
             ; will
             ; TODO so how this will work is that every loop start, we check if any tracks need to be started. If so, start them.
             ; We also stop tracks that need to be stopped.
             (printf "Loop starts\n")
             ; (sleep sleep-time-in-secs)
             (for ([step-no (in-range rs-main-num-divs)])
               (printf "Step\n")
               (sleep rs-main-div-time-in-secs)
               )
             (loop))
           (printf "rs Main loop ends\n")))))

; Void
(define (rs-stop-main-loop!)
  ; Stops the main loop
  (kill-thread rs-main-loop))


