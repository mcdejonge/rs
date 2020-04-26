#lang racket

;; Main rs file. Contains the main loop and functions for interacting with it.

;; This should make the GC work more often, but do less work every
;; time making performance more constant (hopefully).
(collect-garbage 'incremental)

(provide
 rs-set-global-bpm!
 rs-set-global-div-length!
 rs-set-global-num-divs!
 rs-start-main-loop!
 rs-stop-main-loop!)

(require "rs-util.rkt"
         "rs-t.rkt"
         ffi/unsafe/atomic)

(define rs-main-loop '())

(define rs-main-bpm 120)

(define rs-main-num-divs 16)

(define rs-main-div-length 1/4)

; A list of track threads.
(define rs-main-tracks-running '())

; These tracks will start at the next loop start.
; Contains a list of rs-t structs.
(define rs-main-tracks-queued '())

; These tracks will stop at the next loop start.
; Contains a list of indexes
(define rs-main-tracks-stopping '())

; Helper time values for use in sleep

(define rs-main-div-length-ms 0.0)

(define rs-main-div-time-in-secs 0.0)

(define rs-main-loop-time-in-secs 0.0)

(define (rs-main-recalculate-time-values!)
  (set! rs-main-div-length-ms
        (rs-calculate-div-length-ms rs-main-bpm rs-main-div-length))
  (set! rs-main-div-time-in-secs
        (/ rs-main-div-length-ms 1000))
  (set! rs-main-loop-time-in-secs
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

(define/contract (rs-stop-track track-no)
  (-> natural? void)
  ; Add the given index to the list of indexes to stop
  ; at the next main loop iteration start.
  (when (< track-no (length rs-main-tracks-running))
    (when (not (member track-no rs-main-tracks-running))
      (cons track-no rs-main-tracks-stopping))))



; Void
(define (rs-start-main-loop!)
  ; Starts the main loop.
  (rs-main-recalculate-time-values!)
  (set! rs-main-loop
        (thread
         (lambda ()
           (let loop ()

             ; Remove tracks that need to be stopped.
             (start-atomic)
             (for ([track-index rs-main-tracks-stopping])
               (let ((track-to-stop (list-ref rs-main-tracks-running track-index)))
                 (remove track-to-stop rs-main-tracks-running)
                 (thread-send track-to-stop 'stop)))
             (set! rs-main-tracks-stopping '())
             (end-atomic)

             ; Start new tracks as needed.  This is atomic because the
             ; new track queue needs to be empty when this is done. (I
             ; think).
             (start-atomic)
             (for ([track rs-main-tracks-queued])
               (let ((track-thread (rs-t-play! track)))
                 (append rs-main-tracks-running (list track-thread))))
             (set! rs-main-tracks-queued '())
             (end-atomic)

             (sleep rs-main-loop-time-in-secs)
             (displayln "Looping")
             (loop))
           (printf "rs Main loop ends\n")))))

; Void
(define (rs-stop-main-loop!)
  ; Stops the main loop
  (kill-thread rs-main-loop))


