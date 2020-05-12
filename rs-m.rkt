#lang racket

;; This module contains functions for sending MIDI notes to instruments.

(require "rs-midi-core.rkt"
         "rs-e.rkt")

(provide
 rs-m-event-cc
 rs-m-event-play
 rs-m-event-play-chord
 rs-m-list-ports
 rs-m-instr
 rs-m-cc
 rs-m-play
 rs-m-play-chord
 )

(define (rs-m-list-ports)
  ;; Return a list of available MIDI ports.
  (rs-midi-core-list-ports))

; An instr-struct contains a port number (or name). When called as a
; function it opens the port and sends the procedure given as an
; argument, passing it the channel number as an argument.
(struct rs-m-instr-struct (port channel)
  #:property prop:procedure
  (lambda (self procedure)
    (rs-midi-core-close-output!)
    (rs-midi-core-open-out-port! (rs-m-instr-struct-port self))
    (procedure (rs-m-instr-struct-channel self))))

(define (rs-m-valid-midi-port? name-or-num)
  ;; Helper function that checks if the supplied MIDI port is valid.
  (cond [(string? name-or-num) (member name-or-num (rs-m-list-ports))]
        [(natural? name-or-num) (< name-or-num (length (rs-m-list-ports)))]
        [else #f]))

(define (rs-m-valid-midi-value? input)
  ;; Helper function that checks if the supplied input is a valid MIDI
  ;; value (integer between 0 and 127).
  (and (natural? input)
       (> input -1)
       (< input 128)))

(define (rs-m-valid-midi-channel? channel)
  ;; Helper function that checks if the supplied input is a valid MIDI
  ;; channel (integer between 0 and 17).
  (and (natural? channel)
       (> channel 0)
       (< channel 17)))

(define (rs-m-instr port [channel 1])
  ;; Define a new instrument that uses the given MIDI port index and channel.
  ;; Channel is optional and defaults to 1.
  (cond [(and
          (natural? port) ; For consistency, only allow calling ports by index.
          (rs-m-valid-midi-port? port)
          (rs-m-valid-midi-channel? channel))
         (rs-m-instr-struct port channel)]
        [else #f]))

(define (rs-m-valid-notes? notes)
  ;; Helper function that checks if notes is a list of valid MIDI notes.
  (and (list? notes)
       (for/and ([note notes]) (rs-m-valid-midi-value? note))))
  
(define (rs-m-play-chord instr notes note-length-ms [velocity 127])
  ;; Play a chord. notes is a list of midi note numbers. All notes
  ;; have the same length and will be played at the same velocity.
  (cond [(and (rs-m-instr-struct? instr)
              (rs-m-valid-notes? notes)
              (natural? note-length-ms)
              (rs-m-valid-midi-value? velocity))
         (instr (lambda (channel)
                  (map (lambda (note)
                         (rs-midi-core-send-note! note note-length-ms velocity channel)) notes)))]
        [else (printf "Invalid arguments supplied to rs-m-play-chord: ~a ~a ~a ~a\n"
                      instr notes note-length-ms velocity)]))

(define (rs-m-event-play-chord instr notes note-length-ms [velocity 127] #:offset [offset 0])
  ;; Create an rs-e structure that can be used in a sequence for
  ;; playing chord using the supplied instrument.
  (cond [(and (rs-m-instr-struct? instr)
              (rs-m-valid-notes? notes)
              (natural? note-length-ms)
              (> note-length-ms 0)
              (rs-m-valid-midi-value? velocity)
              (and (real? offset)
                   (> offset -1)
                   (< offset 1)))
         (rs-e-create #:fn (lambda (step-time)
                        (rs-m-play-chord instr notes note-length-ms velocity)))]
        [else (printf "Invalid arguments supplied to rs-m-event-play-chord : ~a ~a ~a ~a offset ~a\n"
                      instr notes note-length-ms velocity offset)]))


(define (rs-m-play instr note note-length-ms [velocity 127])
  ;; Play a MIDI note using the supplied instrument.
  (cond [(and (rs-m-instr-struct? instr)
              (rs-m-valid-midi-value? note)
              (natural? note-length-ms)
              (> note-length-ms 0)
              (rs-m-valid-midi-value? velocity))
         (instr (lambda (channel)
                  (rs-midi-core-send-note! note note-length-ms velocity channel)))]
        [else (printf "Invalid arguments supplied to rs-m-play: ~a ~a ~a ~a\n"
                      instr note note-length-ms velocity)]))

(define (rs-m-event-play instr note note-length-ms [velocity 127] #:offset [offset 0])
  ;; Create an rs-e structure that can be used in a sequence for
  ;; playing a MIDI note using the supplied instrument.
  (cond [(and (rs-m-instr-struct? instr)
              (rs-m-valid-midi-value? note)
              (natural? note-length-ms)
              (> note-length-ms 0)
              (rs-m-valid-midi-value? velocity)
              (and (real? offset)
                   (> offset -1)
                   (< offset 1)))
         (rs-e-create #:fn (lambda (step-time)
                        (rs-m-play instr note note-length-ms velocity)))]
        [else (printf "Invalid arguments supplied to rs-m-event-play: ~a ~a ~a ~a offset ~a\n"
                      instr note note-length-ms velocity offset)]))

(define (rs-m-cc instr cc-no cc-val)
  ;; Set the supplied cc number to the supplied cc value for the
  ;; supplied instrument.
  (cond [(and (rs-m-instr-struct? instr)
              (rs-m-valid-midi-value? cc-no)
              (rs-m-valid-midi-value? cc-val))
         (instr (lambda (channel)
                  (rs-midi-core-send-cc! cc-no cc-val channel)))]
        [else (printf "Invalid arguments supplied to rs-m-cc: ~a ~a ~a" instr cc-no cc-val)]))

(define (rs-m-event-cc instr cc-no cc-val #:offset [offset 0])
  ;; Create an rs-e structure that can be used in a sequence for
  ;; sending a MIDI cc message using the supplied instrument.
  (cond [(and (rs-m-instr-struct? instr)
              (rs-m-valid-midi-value? cc-no)
              (rs-m-valid-midi-value? cc-val)
              (and (real? offset)
                   (> offset -1)
                   (< offset 1)))
         (rs-e-create #:fn (lambda (step-time)
                        (rs-m-cc instr cc-no cc-val)))]
        [else (printf "Invalid arguments supplied to rs-m-event-cc: ~a ~a ~a offset ~a\n"
                      instr cc-no cc-val offset)]))


(module+ test
  (define test-info #<<EOF

What should happen: a list of all available MIDI ports should be
printed.

Then, if you have at least one port available on your system, three
notes will be played on the first available port on channel 1.

EOF
    )

  
  (displayln test-info)
  (printf "Available ports:\n~s\n" (rs-m-list-ports))

  (cond [(> (length (rs-m-list-ports)) 0)
         
         (define instr (rs-m-instr 0))
         (rs-m-play instr 65 300 40)
         (sleep 0.3)
         (rs-m-play instr 60 300 80)
         (sleep 0.3)
         (rs-m-play instr 55 300 120)
         (sleep 0.3)
         
         ]
  [else (displayln "No MIDI ports are available for testing.")])

  ;; No test for rs-m-event-play as that would require bringing in too
  ;; much extra stuff. Check the demo.

  ;; No test for rs-m-cc and rs-m-event-cc as they require too much setting up.
  ;; Run rs-tool-cc.rkt if you want to assign cc messages.

  
  )


