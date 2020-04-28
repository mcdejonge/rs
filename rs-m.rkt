#lang racket

;; This module contains functions for sending MIDI notes to instruments.

(require "rs-midi-core.rkt")

(provide
 rs-m-list-ports
 rs-m-instr
 rs-m-play
 )

(define (rs-m-list-ports)
  ; Return a list of available MIDI ports.
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
  ; Helper function that checks if the supplied MIDI port is valid.
  (cond [(string? name-or-num) (member name-or-num (rs-m-list-ports))]
        [(natural? name-or-num) (< name-or-num (length (rs-m-list-ports)))]
        [else #f]))

(define (rs-m-valid-midi-channel? channel)
  (and (natural? channel)
       (> channel 0)
       (< channel 17)))

(define (rs-m-instr port [channel 1])
  ; Define a new instrument that uses the given MIDI port index and channel.
  ; Channel is optional and defaults to 1.
  (cond [(and
          (natural? port) ; For consistency, only allow calling ports by index.
          (rs-m-valid-midi-port? port)
          (rs-m-valid-midi-channel? channel))
         (rs-m-instr-struct port channel)]
        [else #f]))


(define (rs-m-play instr note note-length-ms [velocity 127])
  (cond [(and (rs-m-instr-struct? instr)
              (integer? note)
              (natural? note-length-ms)
              (> note-length-ms 0)
              (natural? velocity)
              (> velocity -1)
              (< velocity 128))
         (instr (lambda (channel)
                  (rs-midi-core-send-note! note note-length-ms velocity channel)))]
        [else (printf "Invalid arguments supplied to rs-m-play: ~a ~a ~a ~a\n"
                      instr note note-length-ms velocity)]))

; TODO add an event for setting CC values.

(module+ test
  (define test-info #<<EOF

What should happen: a list of all available MIDI ports should be
printed.

Then, if you have at least one port available on your system, three
notes will be played on the first available port on channel 1.

EOF
    )

  
  (displayln test-info)
  (printf "Available ports:\n~s" (rs-m-list-ports))

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


  
  )


