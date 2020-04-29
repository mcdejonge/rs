#lang racket

;; Demo (2) for rs, the racket sequencer. This demo shows you can play
;; multiple sequences that each have a different number of steps
;; (polymeter) on different instruments.
;;
;; To prepare, make sure you have at least one MIDI port available and
;; two instruments, one of which listens on channel 1 and the other on
;; channel 2.

(require "rs.rkt"
         "rs-m.rkt")

(when (not (length (list)))
  (printf "No MIDI ports available. This will not work.\n"))

;; Again, set up a simple 128 BPM loop with 16 sub divisions of 1/4 beats
;; each and start it. Note the ! at the end of these function
;; names. This ! means the function will cause something to happen to
;; the main loop.
(rs-set-global-bpm! 128)
(rs-set-global-div-length! 1/4)
(rs-set-global-steps! 8)

(rs-start-main-loop!)


;; Create an instrument on the first channel of the first available
;; MIDI port.
(define instr1 (rs-m-instr 0 1))

;; Create another instrument on the second channel of the first available MIDI port.
(define instr2 (rs-m-instr 0 2))

;; Create events and sequences for the first instrument.
(define i1low (rs-m-event-play instr1 48 100 90))
(define i1hi (rs-m-event-play instr1 60 50 80))
(define i1seq1 (list i1low
                     null
                     i1hi
                     null))
(define i1seq2 (list null
                     i1low
                     i1low
                     i1hi))

;; Create events and sequences for the second instrument.
(define i2low (rs-m-event-play instr2 48 100 90))
(define i2med (rs-m-event-play instr2 47 50 80))
(define i2hi (rs-m-event-play instr2 60 50 70))
(define i2seq1 (list i2low
                     null
                     i2med
                     null
                     i2hi
                     null))
(define i2seq2 (list i2hi
                     i2med
                     i2low))

;; Create tracks for each instrument. They can use the main loop settings for now.
(define track1 (rs-track i1seq1))
(define track2 (rs-track i2seq1))

;; First queue the first track
(rs-queue-track! track1)

;; Wait for a while.
(sleep 2)

;; Introduce the second track
(rs-queue-track! track2)

;; Wait for a while.
(sleep 4)

;; Make the second track play a different sequence (and wait a while).
(set-rs-t-seq! track2 i2seq2)
(sleep 4)

;; Make the first track play a different sequence (and wait a while).
(set-rs-t-seq! track1 i1seq2)
(sleep 4)

;; Stop the first track but leave the second one running.
(rs-stop-track! 0)
(sleep 2)

;; We're done. Stop the main loop. The performance is over.
(rs-stop-main-loop!)



