#lang racket/base

(require "rs-e.rkt"
         "rs-m.rkt"
         "rs-t.rkt"
         "rs-util.rkt"
         racket/contract/base
         racket/contract/region
         racket/math
         ffi/unsafe/atomic)


;; Main rs file. Contains the main loop and functions for interacting with it.


(provide

 rs-main-bpm
 rs-main-div-length
 rs-main-steps

 rs-pause
 rs-queue-track!
 rs-set-global-bpm!
 rs-set-global-div-length!
 rs-set-global-steps!
 rs-stop-track!
 rs-start-main-loop!
 rs-stop-main-loop!

 rs-track

 ; From included files (so you only need to require rs.rkt)
 
 ; From rs-e.rkt
 (struct-out rs-e)
 rs-e-create
 rs-e-multiple

 ; From rs-m.rkt
 rs-m-event-cc
 rs-m-event-play
 rs-m-event-play-chord
 rs-m-list-ports
 rs-m-instr
 rs-m-cc
 rs-m-play
 rs-m-play-chord

 ; From rs-t.rkt
 (struct-out rs-t)
 rs-t-create
 rs-t-play-seq!

 ; From rs-util.rkt
 rs-util-diag
 rs-util-set-diag-mode


 )

(define main-loop '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; Timing and temp functions and variables.                                 ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Timing works as follows: you set the BPM, the length of a single
;; division as a fraction of a single beat and the number of such
;; divisions that make up a single loop.
;;
;; A regular 16 step sequence at 128 BPM would then have the BPM set
;; at 128 (obviously), the division length to 1/4 (because there are
;; four steps for every beat) and the number of divisions to 16.
;;
;; A 3/4 sequence would have the division length set to 1/4 as well and the number
; Beats per minute
(define rs-main-bpm 120)

; Length of a single division that makes up a single loop.
(define rs-main-div-length 1/4)

; Number of divisions that makes up a loop.
(define rs-main-steps 16)


(define main-loop-time-in-msecs 0) ; Keep times in msecs as long as possible to avoid doing floating point math.

(define (rs-main-recalculate-loop-length!)
  (set! main-loop-time-in-msecs
        (/ 60000 rs-main-bpm)))
(rs-main-recalculate-loop-length!)

(define/contract (rs-set-global-steps! steps)
  (-> natural? void)
  ; Set the global number of divs.
  (set! rs-main-steps steps))

(define/contract (rs-set-global-bpm! bpm)
  (-> natural? void)
  ; Set the global BPM.
  (set! rs-main-bpm bpm)
  (rs-main-recalculate-loop-length!))

(define/contract (rs-set-global-div-length! div-length)
  (-> positive? void)
  (set! rs-main-div-length div-length))

(define (valid-num-steps-for-pause? num-steps)
  ;; Helper function for the contract of rs-pause.
  (or (positive? num-steps)
      (= 0 num-steps)))

(define/contract (rs-pause num-loops num-steps)
  (-> natural? valid-num-steps-for-pause? void)
  ;; Pause (ie sleep) for the given number of loops and steps.
  (define num-ms
    (+ main-loop-time-in-msecs
       (* num-steps rs-main-div-length)))
  (rs-util-rtsleep num-ms))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; Queuing and stopping stracks                                             ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct active-track (track track-thread))

;; A list of active-track structs.
(define running-tracks '())

;; These tracks will start at the next loop start.
;; Contains a list of rs-t structs.
(define queued-tracks '())

;l These tracks will stop at the next loop start.
;; Contains a list of rs-t structs.
(define tracks-to-stop '())


(define (track-or-index? arg)
  ;; Helper function that checks if something is either a track or an
  ;; index.
  (or (rs-t? arg)
      (natural? arg)))

(define/contract (rs-queue-track! track)
  (-> rs-t? void)
  ;; Add the given track to the list of tracks to enqueue.
  (rs-util-diag "Queueing track ~s\n" track)
  (set! queued-tracks (cons track queued-tracks)))

(define (get-running-track track search-list)
  ;; Return the active-track struct matching the given track (rs-t). Or nothing.
  (cond [(null? search-list) null]
        [(eq? track (active-track-track (car search-list)))
         (car search-list)]
        [else (get-running-track track (cdr search-list))]))

(define/contract (rs-stop-track! track)
  (-> track-or-index? void)
  ;; Stop the given track. The track can be either a track index or
  ;; the track itself.
  (rs-util-diag "Stopping track ~s\n" track)
  (cond [(and (natural? track)
              (< track (length running-tracks)))
         (define running-track (list-ref running-tracks track))
         (when (not (member running-track tracks-to-stop))
           (set! tracks-to-stop
                 (cons running-track tracks-to-stop)))]
        [(rs-t? track)
         (define running-track (get-running-track track running-tracks))
         (when (and (not (null? running-track))
                    (not (member running-track tracks-to-stop)))
           (set! tracks-to-stop
                 (cons running-track tracks-to-stop)))]
        [else (raise "No such track.")])
  (void))
  
(define (rs-track sequence)
  ; Create a new track that uses the main settings for BPM and divisions.
  (-> rs-t-valid-sequence? rs-t?)
  (rs-t-create #:bpm rs-main-bpm
               #:steps rs-main-steps
               #:div-length rs-main-div-length
               #:seq sequence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; Main loop functions.                                                     ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rs-start-main-loop!)
  ;; Starts the main loop.
  (rs-util-diag "Starting main loop.\n")
  (collect-garbage 'minor)
  (set! main-loop
        (thread
         (lambda ()
           (rs-util-loop-and-wait
            (lambda ()
              ;; This should make the GC work more often, but do less work every
              ;; time making performance more constant (hopefully).
              (collect-garbage 'incremental)
              (collect-garbage 'minor)
              ; Start new tracks as needed.  This is atomic because the
              ; new track queue needs to be empty when this is done. (I
              ; think).
              (thread
               (lambda()
                 (start-atomic)
                 (collect-garbage 'incremental)
                 (for ([track queued-tracks])
                   (set! running-tracks
                         ;; Use append to its easier to deactive threads by index.
                         (append running-tracks
                                 (list
                                  (active-track track (rs-t-play! track))))))
                 (set! queued-tracks '())
                 (end-atomic)
                 ))
              ; Remove tracks that need to be stopped.
              (thread
               (lambda ()
                 (start-atomic)
                 (for ([track-to-stop tracks-to-stop])
                   (thread-send (active-track-track-thread track-to-stop) 'stop))
                 (set! running-tracks
                       (remq* tracks-to-stop running-tracks))
                 (set! tracks-to-stop '())
                 (end-atomic)))


              )
            main-loop-time-in-msecs
            1/20
            )))))

(define (rs-stop-main-loop!)
  ;; Stops the main loop and all running tracks.
  (rs-util-diag "Stopping maing loop.\n")
  (for ([running-track running-tracks])
    (thread-send (active-track-track-thread running-track) 'stop))
  (set! running-tracks '())
  (kill-thread main-loop))

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
