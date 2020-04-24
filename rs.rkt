#lang racket

;; Main rs file. Contains the main loop and functions for interacting with it.

(provide
 rs-set-global-division-length!
 rs-set-global-num-divisions!
 rs-start-main-loop!
 rs-stop-main-loop!)

(define rs-main-loop '())

(define rs-main-bpm 120)

(define rs-main-num-divisions 16)

(define rs-main-division-length 1/4)

(define rs-channels '())

; Helper timevalues for use in sleep

(define rs-main-division-length-ms 0.0)

(define rs-main-division-time-in-secs 0.0)

(define rs-main-sleep-time-in-secs 0.0)

; Int, num -> float
(define (rs-calculate-division-length-ms bpm division-length)
  ; Calculate the length of a division in ms.
  (* (* (/ 60000 bpm) division-length) 1.0))

(define (rs-main-recalculate-time-values!)
  (set! rs-main-division-length-ms
        (rs-calculate-division-length-ms rs-main-bpm rs-main-division-length))
  (set! rs-main-division-time-in-secs
        (/ rs-main-division-length-ms 1000))
  (set! rs-main-sleep-time-in-secs
    (* rs-main-num-divisions rs-main-division-time-in-secs)))

(define/contract (rs-set-global-num-divisions! num-divisions)
  (-> positive? void)
  ; Set the global number of divisions.

  (set! rs-main-num-divisions num-divisions)
  (rs-main-recalculate-time-values!))

(define/contract (rs-set-global-division-length! division-length)
  (-> positive?)
  (set! rs-main-division-length division-length)
  (rs-main-recalculate-time-values!))


; Void
(define (rs-start-main-loop!)
  ; Starts the main loop.
  (rs-main-recalculate-time-values!)
  (set! rs-main-loop
        (thread
         (lambda ()
           (let loop ()
             (printf "Loop starts\n")
             ; (sleep sleep-time-in-secs)
             (for ([step-no (in-range rs-main-num-divisions)])
               (printf "Step\n")
               (sleep rs-main-division-time-in-secs)
               )
             (loop))
           (printf "rs Main loop ends\n")))))

; Void
(define (rs-stop-main-loop!)
  ; Stops the main loop
  (kill-thread rs-main-loop))


