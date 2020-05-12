#lang racket

;; Demo that shows how you can play chords. Make sure you have a
;; polyphonic instrument set up to receive notes on channel 1 of the
;; first available MIDI port.

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
(define instr (rs-m-instr 0 1))

;; Define two events for playing chords. Note how chords are lists of
;; MIDI note values.
(define chord1 (rs-m-event-play-chord instr (list 60 65 69) 90 110))
(define chord2 (rs-m-event-play-chord instr (list 60 66 69) 175 90))

;; Create a sequence that alternates the two chords.
(define sequence (list chord1 '() chord2 '()))

;; Create a track that uses the sequence and it.
(define track (rs-track sequence))

(rs-queue-track! track)

;; After five seconds, stop playing this sequence.
(sleep 5)
(rs-stop-main-loop!)
