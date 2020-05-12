#lang racket

;; Demo that shows how you can play polyrhythms. This is made possible
;; by the fact that rs allows you to set different lengths per track.
;;
;; This demo uses two instruments set up to receive notes on channel 1
;; and channel 2 respectively of the first available MIDI port. The
;; instrument on channel 1 will play in 4/4 and the instrument on
;; channel 2 will play in 5/4.

(require "../rs.rkt"
         "../rs-m.rkt")

(when (not (length (list)))
  (printf "No MIDI ports available. This will not work.\n"))

(rs-set-global-bpm! 128)
(rs-set-global-div-length! 1/4)
(rs-set-global-steps! 4)

(rs-start-main-loop!)

;; Set up an instrument on the first channel of the first available
;; MIDI port.
(define instr1 (rs-m-instr 0 1))
;; Set up an instrument on the second channel of the first available
;; MIDI port.
(define instr2 (rs-m-instr 0 2))

;; Set up events for the two notes we're going to play.
(define note1 (rs-m-event-play instr1 60 80 90))
(define note2 (rs-m-event-play instr2 48 100 110))

;; Create two sequences, one a 4/4 sequence and ane a 5/4 sequence.
(define seq44 (list note1 '() note1 '()))
(define seq54 (list note2 '() note2 '() note2))

;; Now create a track for the 44 sequence and queue it.
;; Note it will use the main settings set at the top of this file
;; (128BPM, 4 steps of 1/4 beat each).
(define track44 (rs-track seq44))
(rs-queue-track! track44)

;; And create a track for the 54 sequence and queue it.  This time we
;; need to set the length of the loop ourselves. For this we can not
;; use the rs-track function. Instead we need to call the rs-t-create
;; function (rs-track is a wrapper around this function). Note we
;; still copy over the main bpm and the main division (step) length.
(define track54 (rs-t-create #:bpm rs-main-bpm
                             #:steps 5
                             #:div-length rs-main-div-length
                             #:seq seq54))
(rs-queue-track! track54)

;; After five seconds, stop playing the two sequences.
(sleep 5)
(rs-stop-main-loop!)
