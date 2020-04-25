#lang racket

;; Code for dealing with channels.

(require "rs-util.rkt")

(provide rs-c
         rs-c-create
         rs-c-play-single-loop)

(struct rs-c (bpm
              num-divisions
              division-length
              sequence) #:transparent) ; TODO lookup if transparency has drawbacks.

; Int, int, positive -> rs-c
(define (rs-c-create #:bpm bpm
                     #:num-divisions [num-divisions 16]
                     #:division-length [division-length 1/4]
                     #:sequence [sequence '()])
  ; TODO this contract does not seem to be working. Might be because
  ; we're not testing across module boundaries.
  (->* (#:bpm positive?)
      (#:num-divisions positive?
       #:division-length positive?
       #:sequence list?)
      any)
  (rs-c bpm num-divisions division-length sequence))

(define (rs-c-get-loop-length-ms channel)
  ; Return the channel loop length in ms)
  (-> rs-c? real?)
  (* (rs-calculate-division-length-ms (rs-c-bpm channel)
                                      (rs-c-division-length channel))
     (rs-c-num-divisions channel)))

(define (rs-c-play-single-loop channel)
  ; Play a single iteration of the current sequence for the channel.
  ; TODO all these calculations should be optimized so they're only done
  ; when the channel changes.
  (-> rs-c? void)
  (let* ((loop-length-ms (rs-c-get-loop-length-ms channel))
         (seq-item-length-ms (/ loop-length-ms (length (rs-c-sequence channel))))
         (seq-item-length-s (/ seq-item-length-ms 1000)))
    
    (for ([seq-item (rs-c-sequence channel)])
      (when (procedure? seq-item) (seq-item))
      (sleep seq-item-length-s)
      )
    ))

;(define (rs-c-play channel)
;  (-> rs-c? thread?)
  ; Return a thread that plays continuously.
  ; TODO this does not allow changing the channel values.
;  (thread (lambda)
;          (let loop ())
;          )
;  )
