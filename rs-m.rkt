#lang racket/base

;; This module contains functions for sending MIDI notes to instruments.

(require racket/contract/base
         racket/contract/region
         racket/math
         "rs-e.rkt"
         "rs-midi-core.rkt")

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; Helper functions for checking parameters. Private.                        ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (valid-midi-port? name-or-num)
  ;; Helper function that checks if the supplied MIDI port is valid.
  (cond [(string? name-or-num) (member name-or-num (rs-m-list-ports))]
        [(natural? name-or-num) (< name-or-num (length (rs-m-list-ports)))]
        [else #f]))

(define (valid-midi-value? input)
  ;; Helper function that checks if the supplied input is a valid MIDI
  ;; value (integer between 0 and 127).
  (and (natural? input)
       (> input -1)
       (< input 128)))

(define (valid-midi-channel? channel)
  ;; Helper function that checks if the supplied input is a valid MIDI
  ;; channel (integer between 0 and 17).
  (and (natural? channel)
       (> channel 0)
       (< channel 17)))

(define (valid-notes? notes)
  ;; Helper function that checks if notes is a list of valid MIDI notes.
  (and (list? notes)
       (for/and ([note notes]) (valid-midi-value? note))))

(define (valid-note-length? note-length-ms)
  ;; Helper function that checks if a note length is valid.
  (and
   (number? note-length-ms)
   (> note-length-ms 0)))

(define (valid-note-length-event? note-length-ms)
  ;; Helper function that checks if a note length for an event (either
  ;; a number > 0 or 0).
  (and
   (number? note-length-ms)
   (>= note-length-ms 0)))

(define (valid-offset? offset)
  ;; Helper function that checks if an event offset is valid.
  (and (real? offset)
       (> offset -1)
       (< offset 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; Creating instruments.                                                     ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (rs-m-instr port [channel 1])
  (->* (valid-midi-port?)
       (valid-midi-value?)
       rs-m-instr-struct?)
  ;; Define a new instrument that uses the given MIDI port (name or
  ;; index) and channel. 
  (rs-m-instr-struct port channel))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; Playing things and sending MIDI commands.                                 ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define/contract (rs-m-play-chord instr notes note-length-ms [velocity 127])
  (->* (rs-m-instr-struct? valid-notes? valid-note-length?)
       (valid-midi-value?)
       any)
  ;; Play a chord. notes is a list of midi note numbers. All notes
  ;; have the same length and will be played at the same velocity.
  (instr (lambda (channel)
           (map (lambda (note)
                  (rs-midi-core-send-note! note (inexact->exact (round note-length-ms)) velocity channel)) notes))))

(define/contract
  (rs-m-event-play-chord instr notes [note-length-ms 0] [velocity 127] #:offset [offset 0])
  (->* (rs-m-instr-struct? valid-notes?)
       (valid-note-length-event? valid-midi-value? #:offset valid-offset?)
       rs-e?)

  (rs-e-create #:fn (lambda (step-time)
                      (rs-m-play-chord instr notes
                                       (if (> note-length-ms 0)
                                           note-length-ms
                                           step-time) velocity))
               #:offset offset))


(define/contract (rs-m-play instr note note-length-ms [velocity 127])
  (->* (rs-m-instr-struct? valid-midi-value? valid-note-length?)
       (valid-midi-value?)
       any)
  ;; Play a single MIDI note using the supplied MIDI instrument.
  (instr (lambda (channel)
           (rs-midi-core-send-note! note (inexact->exact(round note-length-ms)) velocity channel))))

(define/contract
  (rs-m-event-play instr note [note-length-ms 0] [velocity 127] #:offset [offset 0])
  (->* (rs-m-instr-struct? valid-midi-value?)
       (valid-note-length-event? valid-midi-value? #:offset valid-offset?)
       rs-e?)
  ;; Create an rs-e structure that can be used in a sequence for
  ;; playing a MIDI note using the supplied instrument.
  (rs-e-create #:fn (lambda (step-time)
                      (rs-m-play instr note
                                 (if (> note-length-ms 0)
                                     note-length-ms
                                     step-time)
                                 velocity))
               #:offset offset))

(define/contract (rs-m-cc instr cc-no cc-val)
  (-> rs-m-instr-struct? valid-midi-value? valid-midi-value? any)
  ;; Set the supplied cc number to the supplied cc value for the
  ;; supplied instrument.
  (instr (lambda (channel)
           (rs-midi-core-send-cc! cc-no cc-val channel))))

(define/contract (rs-m-event-cc instr cc-no cc-val #:offset [offset 0])
  (->* (rs-m-instr-struct? valid-midi-value? valid-midi-value?)
       (#:offset valid-offset?)
       rs-e?)
  ;; Create an rs-e structure that can be used in a sequence for
  ;; sending a MIDI cc message using the supplied instrument.
  (rs-e-create #:fn (lambda (step-time)
                      (rs-m-cc instr cc-no cc-val))
               #:offset offset))


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


