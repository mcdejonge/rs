#lang racket

;; Main rs file. Contains the main loop and functions for interacting with it.

;; This should make the GC work more often, but do less work every
;; time making performance more constant (hopefully).
(collect-garbage 'incremental)

(provide

 rs-queue-track!
 rs-set-global-bpm!
 rs-set-global-div-length!
 rs-set-global-num-divs!
 rs-stop-track!
 rs-start-main-loop!
 rs-stop-main-loop!

 rs-track

 ; From included files (so you only need to require rs.rkt)
 
 ; From rs-e.rkt
 (struct-out rs-e)
 rs-e-create

 ; From rs-t.rkt
 (struct-out rs-t)
 rs-t-create
 rs-t-play!
 rs-t-play-seq!

 )

(require
 "rs-e.rkt"
 "rs-t.rkt"
 "rs-util.rkt"
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

(define rs-main-loop-time-in-secs 0.0)

(define (rs-main-recalculate-loop-length!)
  (set! rs-main-loop-time-in-secs
        (/ 60.0 rs-main-bpm)))

;; TODO contracts may seem nice, but they cause the program to stop,
;; which is not what you want in a sequencer. For public functions
;; just output an error message, refuse to do something stupid and
;; continue.

(define/contract (rs-set-global-num-divs! num-divs)
  (-> natural? void)
  ; Set the global number of divs.
  (set! rs-main-num-divs num-divs))

(define/contract (rs-set-global-bpm! bpm)
  (-> natural? void)
  ; Set the global BPM.
  (set! rs-main-bpm bpm)
  (rs-main-recalculate-loop-length!))

(define/contract (rs-set-global-div-length! div-length)
  (-> positive? void)
  (set! rs-main-div-length div-length))


(define/contract (rs-queue-track! track)
  (-> rs-t? void)
  ; Add the given track to the list of tracks to enqueue.
  (set! rs-main-tracks-queued (cons track rs-main-tracks-queued)))

(define/contract (rs-stop-track! track-no)
  (-> natural? void)
  ; Add the given index to the list of indexes to stop
  ; at the next main loop iteration start.
  (when (< track-no (length rs-main-tracks-running))
    (when (not (member track-no rs-main-tracks-stopping))
      (set! rs-main-tracks-stopping (cons track-no rs-main-tracks-stopping)))))


(define (rs-track sequence)
  ; Create a new track that uses the main settings for BPM and divisions.
  (-> rs-t-valid-sequence? rs-t?)
  (rs-t-create #:bpm rs-main-bpm
               #:num-divs rs-main-num-divs
               #:div-length rs-main-div-length
               #:seq sequence))


; Void
(define (rs-start-main-loop!)
  ; Starts the main loop.
  (set! rs-main-loop
        (thread
         (lambda ()
           (let loop ()

             (thread
              (lambda ()
                ; Remove tracks that need to be stopped.
                (start-atomic)
                (for ([track-index rs-main-tracks-stopping])
                  (let ((track-to-stop (list-ref rs-main-tracks-running track-index)))
                    (set! rs-main-tracks-running (remove track-to-stop rs-main-tracks-running))
                    (thread-send track-to-stop 'stop)))
                (set! rs-main-tracks-stopping '())
                (end-atomic)))

             ; Start new tracks as needed.  This is atomic because the
             ; new track queue needs to be empty when this is done. (I
             ; think).
             (thread
              (lambda()
                (start-atomic)
                (for ([track rs-main-tracks-queued])
                  (let ((track-thread (rs-t-play! track)))
                    (set! rs-main-tracks-running
                          (append rs-main-tracks-running (list track-thread)))))
                (set! rs-main-tracks-queued '())
                (end-atomic)
                ))
             (sleep rs-main-loop-time-in-secs)
             (loop))))))

; Void
(define (rs-stop-main-loop!)
  ; Stops the main loop and all running tracks.
  (for ([track-thread rs-main-tracks-running])
    (thread-send track-thread 'stop))
  (set! rs-main-tracks-running '())
  (kill-thread rs-main-loop))

(module+ test

  (define (rs-test)
    (displayln "What you should see: first only Event 1, then Event 2 interspersed with Event 1 and finally only Event 1 again. Then everything should stop.")
    (let* ([event1
            (rs-e-create
             #:fn (lambda (step-time)
                    (printf "Event 1 with step time ~a\n" step-time)))]
           [event2
            (rs-e-create
             #:fn (lambda (step-time)
                    (printf "Event 2 with step time ~a\n" step-time)))]
           [sequence1
            (list '() event1 '() event1 '())]
           [sequence2
            (list '() event2 '() event2 '() event2)]
           [track1 (rs-track sequence1)]
           [track2
            (rs-t-create #:bpm 196 #:seq sequence2)])
      (rs-start-main-loop!)
      (rs-queue-track! track1)
      (sleep 4)
      (rs-queue-track! track2)
      (sleep 4)
      (rs-stop-track! 1)
      (sleep 6)
      (rs-stop-main-loop!)

      ))
  (rs-test)
  )
