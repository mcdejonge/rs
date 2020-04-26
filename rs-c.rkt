#lang racket

;; Code for dealing with channels.

(require "rs-util.rkt")

(provide rs-c
         rs-c-create
         rs-c-play
         rs-c-play-seq)


(struct rs-c (bpm
              num-divs
              div-length
              seq)
  #:mutable #:transparent) ; TODO lookup if transparency has drawbacks.

; Int, int, positive -> rs-c
(define (rs-c-create #:bpm bpm
                     #:num-divs [num-divs 16]
                     #:div-length [div-length 1/4]
                     #:seq [seq '()])
  ; TODO this contract does not seem to be working. Might be because
  ; we're not testing across module boundaries.
  (->* (#:bpm positive?)
      (#:num-divs positive?
       #:div-length positive?
       #:seq list?)
      any)
  (rs-c bpm num-divs div-length seq))

(define (rs-c-get-loop-length-ms channel)
  ; Return the channel loop length in ms)
  (-> rs-c? real?)
  (* (rs-calculate-div-length-ms (rs-c-bpm channel)
                                      (rs-c-div-length channel))
     (rs-c-num-divs channel)))

(define (rs-c-play-single-loop channel)
  ; Play a single iteration of the current seq for the channel.
  ; TODO all these calculations should be optimized so they're only done
  ; when the channel changes.
  (-> rs-c? void)
  (rs-c-play-seq #:length-in-ms (rs-c-get-loop-length-ms channel) #:seq (rs-c-seq channel)))

(define (rs-c-play-seq #:length-in-ms length-in-ms #:seq seq)
  (-> positive? list?)
  ; Space the items in the seq evenly among the available time and call them.
  (let ((seq-item-length-s (/ (/ length-in-ms (length seq)) 1000)))
    (for ([seq-item seq])
      (when (procedure? seq-item) (thread seq-item))
      (sleep seq-item-length-s))))

(define (rs-c-test)
  (let* ([event
          (lambda()
                  (printf "Event is called\n"))]
         [channel
          (rs-c-create #:bpm 128 #:seq (list '() event '() event event '()))])
    (rs-c-play channel)))


(define (rs-c-play channel)
  (-> rs-c? thread?)
  ; Return a thread that plays continuously until it receives a 'stop message.
  (thread
   (lambda ()
     (let loop()
       (rs-c-play-single-loop channel)
       (match (thread-try-receive)
         [(? rs-c? new-channel-info)
          (set! channel new-channel-info)
          (loop)]
         [ 'stop
           (void)]
         [ #f (loop)])))))
      
