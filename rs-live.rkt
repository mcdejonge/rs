#lang racket

;; Skeleton for live coding rs. Run this file to set things up in your
;; REPL.

(require "rs.rkt"
         "rs-m.rkt")

(when (not (length (list)))
  (printf "No MIDI ports available. This will not work.\n"))

(rs-set-global-bpm! 128)
(rs-set-global-div-length! 1/4)
(rs-set-global-steps! 4)

(rs-start-main-loop!)

;; Here's an example of an instrument. This one runs on channel 1 of
;; the first available MIDI port.
(define instr1 (rs-m-instr 0 1))

;; Here's an example of an event that uses instr1
(define test-note (rs-m-event-play instr1 48 100 90))

;; If you want, you can send it to the MIDI instrument directly:
((rs-e-fn test-note) 100) ;; (the parameter is not used yet).

;; A sequence is a simple list:
(define sequence1 (list test-note
                        '()
                        '()
                        test-note))

;; To run a sequence, create a track that uses it and queue it.
(define track1 (rs-track sequence1))

(rs-queue-track! track1)

;; After a while, you may wish to set a different sequence:
(set-rs-t-seq! track1 (list '()
                            test-note
                            '()
                            test-note
                            '())) ;; That's right, 5 notes in the same time!

;; Want to know what else you can do? See rs-demo2 for an example of
;; adding tracks and stopping tracks. Have fun!
