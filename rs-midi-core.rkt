#lang racket

;; Core MIDI related functionality for rs. Wraps the *true* core MIDI
;; functionality in an interface.
;;
;; Do not call these functions directly when sequencing. Use the
;; higher level rs-m instead.
;;
;; Currently based on RtMidi
;; (https://docs.racket-lang.org/rtmidi/index.html)


(provide rs-midi-core-open-out-port!
         rs-midi-core-close-output!
         rs-midi-core-list-ports
         rs-midi-core-print-port-list
         rs-midi-core-send-note!)


;; To make this work, you must link
;; /Users/matthijs/Library/Racket/7.5/pkgs/rtmidi/rtmidi/wrap-rtmidi.dylib
;; to the directory of this file.
;; TODO maybe fork rtmidi and fix the path to the dylib

;; RtMidi is not documented. Code is here
;; https://github.com/jbclements/rtmidi/blob/master/rtmidi/main.rkt

(require rtmidi)

;; Setup

; Create RtMidiIn and RtMidiOut
(define in (make-rtmidi-in))
(define out (make-rtmidi-out))

(define in-ports (rtmidi-ports in))
(define out-ports (rtmidi-ports out))


; List input and output ports

(define (rs-midi-core-list-ports)
  ; Return a list of available ports.
  out-ports)

; Void
(define (rs-midi-core-print-port-list)
  ; Print a list of available ports to STDOUT.
  (printf "Input ports: ~a~n" in-ports)
  (printf "Output ports: ~a~n" out-ports))

; Void
(define (rs-midi-core-close-output!)
  ; Close the currently active output port.
  (rtmidi-close-port out))

(define (string-or-natural? input)
  ; Helper function to use in a contract.
  (or (string? input) (natural? input)))

; String or int -> Void 
(define/contract (rs-midi-core-open-out-port! name-or-index)
  ;; Open a MIDI out port by name or index and closes any other open ports
  ;; as there can be only one opened at the same time.
  (-> string-or-natural? void)
  (cond
    [(string? name-or-index) (rs-midi-core-open-out-port-by-name! name-or-index)]
    [(natural? name-or-index) (rs-midi-core-open-out-port-by-index! name-or-index)]
    [else (raise-argument-error 'name-or-index "string? or natural?" name-or-index)]))

; String -> Void
(define (rs-midi-core-open-out-port-by-name! name)
 ;; Open a MIDI out port by name and closes any other open ports
  ;; as there can be only one opened at the same time.
  (let ((port-num (index-of (rtmidi-ports in) name)))
    (cond
      [(equal? #f port-num) (error "No MIDI port with name: " name)]
      [else
       (rtmidi-close-port out)
       (rtmidi-open-port out port-num)])))

; Int -> Void
(define (rs-midi-core-open-out-port-by-index! index)
  (cond
    [(< index (length out-ports)) (rtmidi-close-port out)
                                   (rtmidi-open-port out index)]
    [else (error "No such MIDI port: " index)]))

; Helper functions for the rs-midi-core-send-note! contract.
(define (midi-value? input)
  (and (natural? input)
       (> input 0)
       (< input 128)))

(define (midi-channel-number? input)
  (and (natural? input)
       (> input  0)
       (< input 17)))

; Int, Natural, Int (0-127), Int (1-16) -> Thread
(define/contract (rs-midi-core-send-note! pitch duration-ms [velocity 127] [channel 1])
  (->* (midi-value? natural?)
      (midi-value? midi-channel-number?) thread?)
  ; Send a note to the currently open port (if any) on the given
  ; channel for the given duration.
  (thread (lambda()
            (rtmidi-send-message out (list (+ channel 143) pitch velocity ))
            (sleep (* (/ 1.0 1000) duration-ms))
            (rtmidi-send-message out (list (+ channel 127) pitch velocity)))))

;; TODO figure out how to send MIDI cc messages and implement sending them.

(define/contract (rs-midi-core-send-cc! cc-no cc-val [channel 1])
  ; Send a MIDI CC message.
  ; TODO create a proper contract.
  (->* (midi-value? midi-value?)
       (midi-channel-number?)
       thread?)
  (printf "~s\n" (list (+ channel 175) cc-no cc-val) )
  (thread (lambda()
            (rtmidi-send-message out (list (+ channel 175) cc-no cc-val)))))
  
(module+ test
  (define test-info #<<EOF

What should happen: a list of all available MIDI ports should be printed.

Then, if you have at least one port available on your
system, a middle C will be played for half a second at full velocity
on channel 1.

EOF
)
  (displayln test-info)

  (rs-midi-core-print-port-list)
  (cond
    [(length out-ports)
     (displayln "At least one port available. Playing a note.")
     (rs-midi-core-open-out-port! 0)
     (rs-midi-core-send-note! 60 500)
     (sleep 1) ; Necessary because otherwise the port may be closed before the note is played.
     (rs-midi-core-close-output!)]
    [else (displayln "No MIDI port available for testing.")]
    )

  
  ;; (rs-midi-core-open-out-port! 0)
  ;; (let loop()
  ;;   (rs-midi-core-send-cc! 7 100)
  ;;   (sleep 0.01)
  ;;   (rs-midi-core-send-note! 60 100)
  ;;   (sleep 0.1)
  ;;   (rs-midi-core-send-cc! 7 120)
  ;;   (rs-midi-core-send-note! 60 100)
  ;;   (sleep 0.2)
  ;;   (printf "Sending CC\n")
  ;;   (loop))

  )
