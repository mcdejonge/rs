#lang racket

;; Tool for assigning cc messages to settings in your DAW. Set the cc
;; number you want to use and make sure the MIDI port id and channel
;; number are set correctly. Then run this script. For 10 seconds it
;; will "wiggle" the value of the selected cc so your DAW can
;; recognize it.

(define cc-no 12) ;; <- The cc number.
(define midi-port 0) ;; <- The index of the MIDI port to use.
(define midi-channel 1) ;; <- The MIDI channel to use.


(require "rs-m.rkt")



(printf "Available ports:\n~s\n" (rs-m-list-ports))

(when (> (+ midi-port 1) (length (rs-m-list-ports)))
  (printf "Sending MIDI CC messages will not work as the selected MIDI port is not available.\n\n"))

(printf "Sending MIDI cc messages for CC ~s on MIDI port ~s and channel ~s\n"
        cc-no midi-port midi-channel)

(define instr (rs-m-instr midi-port midi-channel))

(for ([i (in-range 100)])
  (rs-m-cc instr cc-no 80)
  (sleep 0.1)
  (rs-m-cc instr cc-no 110))

(printf "Done\n")
