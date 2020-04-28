
#lang racket

;; Run multiple sequences to diagnose timing problems.
;; Sequences are:
;; X - - - 
;; X - X - 
;; - - X -
;;
;; By sending these to your DAW and recording them you can determine
;; by how much the timing of rs is off and / or drifts over time.
;;
;; To prepare, make sure you have at least one MIDI port available and
;; three instruments, one of which listens on channel 1, the other on
;; channel 2 and the third on channel 3

(require "rs.rkt"
         "rs-m.rkt")

(when (not (length (list)))
  (printf "No MIDI ports available. This will not work.\n"))

;; Again, set up a simple 128 BPM loop with 16 sub divisions of 1/4 beats
;; each and start it. Note the ! at the end of these function
;; names. This ! means the function will cause something to happen to
;; the main loop.
(rs-set-global-bpm! 128)
(rs-set-global-div-length! 1/8)
(rs-set-global-num-divs! 16)

(rs-start-main-loop!)


;; Create instruments.
(define instr1 (rs-m-instr 0 1))
(define instr2 (rs-m-instr 0 2))
(define instr3 (rs-m-instr 0 3))

;; Create events and sequences.
(define i1note (rs-m-event-play instr1 48 100 90))
(define i2note (rs-m-event-play instr2 55 100 90))
(define i3note (rs-m-event-play instr3 60 100 90))

(define i1seq (list i1note
                    null
                    null
                    null))
(define i2seq (list i2note
                    null
                    i2note
                    null))
(define i3seq (list null
                    null
                    i3note
                    null))



;; Create tracks for each instrument. They can use the main loop settings for now.
(define track1 (rs-track i1seq))
(define track2 (rs-track i2seq))
(define track3 (rs-track i3seq))

;; Cue and stop the tracks.
(rs-queue-track! track1)
(sleep 4.0)

(rs-queue-track! track2)
(sleep 4.0)

(rs-queue-track! track3)
(sleep 4.0)

(rs-stop-track! 0)
(sleep 4.0)

(rs-stop-track! 0)
(sleep 4.0)

(rs-stop-track! 0)

;; We're done. Stop the main loop. The performance is over.
(rs-stop-main-loop!)



