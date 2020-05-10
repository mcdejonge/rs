#lang racket

;; Demo (1) for rs, the racket sequencer. This demo plays a simple
;; monophonic sequence on channel 1 of the first available MIDI device.

(require "../rs.rkt"
         "../rs-m.rkt")

(printf "Available MIDI ports: ~s\n" (rs-m-list-ports))
(when (not (length (rs-m-list-ports)))
  (printf "No MIDI ports available. This will not work.\n"))

;; Set up a simple 128 BPM loop with 16 sub divisions of 1/4 beats
;; each and start it. Note the ! at the end of these function
;; names. This ! means the function will cause something to happen to
;; the main loop.
(rs-set-global-bpm! 128)
(rs-set-global-div-length! 1/4)
(rs-set-global-steps! 16)

(rs-start-main-loop!)


;; Create an instrument that uses the first available MIDI port. To
;; use another one, supply its index as the first argument to
;; rs-m-instr.
(define instr (rs-m-instr 0))


;; Create an event that plays a low note on the instrument for 100ms
;; and at 90 velocity.
(define low (rs-m-event-play instr 48 100 90))

;; Create an event that plays a high note on the instrument for 50ms and at 80 velocity.
(define hi (rs-m-event-play instr 60 50 80))

;; Create a sequence that alternates the low and the high notes, with an empty division inbetween.
(define sequence (list low
                       null
                       hi
                       null))

;; Create a new track that uses the main loop settings.
(define track (rs-track sequence))

;; Enqueue the new track. 
(rs-queue-track! track)

;; Wait for a while.
(sleep 4)

;; Stop playing the track. Note we refer to it by index.
(rs-stop-track! 0)

(sleep 2)

;; Add an extra event to the sequence of the track.
;; First update the sequence itself.
(set! sequence (append sequence (list hi)))
;; Then update the sequence assigned to the track.
(set-rs-t-seq! track sequence)

;; Let's queue the track again
(rs-queue-track! track)

;; Notice the sequence is now playing faster than before. The reason
;; is the length of a single loop has stayed the same but now we need
;; to play five events in this time whereas before there were only
;; four.

;; Wait for a while.
(sleep 4)

;; And stop the main loop. The performance is over.
(rs-stop-main-loop!)


