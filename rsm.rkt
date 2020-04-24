#lang racket

  
(provide rs-m-open-out-port
         rs-m-close-output
         rs-m-print-port-list
         rs-m-send-note)


;; To make this work, you must link
;; /Users/matthijs/Library/Racket/7.5/pkgs/rtmidi/rtmidi/wrap-rtmidi.dylib
;; to the directory of this file.

;; RtMidi is not documented. Code is here
;; https://github.com/SamuelToups/racket-rtmidi/blob/master/rtmidi.rkt

(require rtmidi)

;; Setup

; Create RtMidiIn and RtMidiOut
(define in (make-rtmidi-in))
(define out (make-rtmidi-out))

(define in-ports (rtmidi-ports in))
(define out-ports (rtmidi-ports out))


; List input and output ports

; Void
(define (rs-m-print-port-list)
  ; Print a list of available ports to STDOUT.
  (printf "Input ports: ~a~n" in-ports)
  (printf "Output ports: ~a~n" out-ports))

; Void
(define (rs-m-close-output)
  ; Close the currently active output port.
  (rtmidi-close-port out))

; String or int -> Void 
(define (rs-m-open-out-port name-or-index)
  ;; Open a MIDI out port by name or index and closes any other open ports
  ;; as there can be only one opened at the same time.
  (cond
    [(string? name-or-index) (rs-m-open-out-port-by-name name-or-index)]
    [(natural? name-or-index) (rs-m-open-out-port-by-index name-or-index)]
    [else (raise-argument-error 'name-or-index "string? or natural?" name-or-index)]))

; String -> Void
(define (rs-m-open-out-port-by-name name)
 ;; Open a MIDI out port by name and closes any other open ports
  ;; as there can be only one opened at the same time.
  (let ((port-num (index-of (rtmidi-ports in) name)))
    (cond
      [(equal? #f port-num) (error "No MIDI port with name: " name)]
      [else
       (rtmidi-close-port out)
       (rtmidi-open-port out port-num)])))

; Int -> Void
(define (rs-m-open-out-port-by-index index)
  (cond
    [(< index (length out-ports)) ((rtmidi-close-port out)
                                   (rtmidi-open-port out index))]
    [else (error "No such MIDI port: " index)]))


; Void
(define (rs-m-send-note pitch duration-ms [velocity 127] [channel 1])
  ; Send a note to the currently open port (if any) on the given
  ; channel for the given duration.
  (thread (lambda()
            (rtmidi-send-message out (list (+ channel 143) pitch velocity ))
            (sleep (* (/ 1.0 1000) duration-ms))
            (rtmidi-send-message out (list (+ channel 127) pitch velocity)))))


(module* main #f
  (rs-m-print-port-list))
