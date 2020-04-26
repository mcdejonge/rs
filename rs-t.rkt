#lang racket

;; Code for dealing with tracks.

(require "rs-util.rkt")

(provide rs-t
         rs-t-create
         rs-t-play
         rs-t-play-seq)


(struct rs-t (bpm
              num-divs
              div-length
              seq)
  #:mutable #:transparent) ; TODO lookup if transparency has drawbacks.

; Int, int, positive -> rs-t
(define/contract (rs-t-create #:bpm bpm
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
  (rs-t bpm num-divs div-length seq))

(define/contract (rs-t-get-loop-length-ms track)
  ; Return the track loop length in ms)
  (-> rs-t? real?)
  (* (rs-calculate-div-length-ms (rs-t-bpm track)
                                      (rs-t-div-length track))
     (rs-t-num-divs track)))

(define/contract (rs-t-play-single-loop track)
  ; Play a single iteration of the current seq for the track.
  ; TODO all these calculations should be optimized so they're only done
  ; when the track changes.
  (-> rs-t? void)
  (rs-t-play-seq #:length-in-ms (rs-t-get-loop-length-ms track) #:seq (rs-t-seq track)))

(define/contract (rs-t-play-seq #:length-in-ms length-in-ms #:seq seq)
  (->* (#:length-in-ms positive?
        #:seq list?)
       void)
  ; Space the items in the seq evenly among the available time and call them.
  ; Each event gets the step length as a parameter. Step length is in seconds.
  (let ((seq-item-length-s (/ (/ length-in-ms (length seq)) 1000)))
    (for ([seq-item seq])
      (when (procedure? seq-item) (thread (lambda ()
                                            (seq-item seq-item-length-s))))
      (sleep seq-item-length-s))))


(define (rs-t-play track)
  (-> rs-t? thread?)
  ; Return a thread that plays continuously until it receives a 'stop message.
  (thread
   (lambda ()
     (let loop()
       (rs-t-play-single-loop track)
       (match (thread-try-receive)
         ; If all you want to do is change the sequence, you do not
         ; need to send a new track as the new sequence is picked up
         ; automatically. You only need this if you want to replace
         ; the currently running track with another.
         [(? rs-t? new-track-info)
          (set! track new-track-info)
          (loop)]
         [ 'stop
           (void)]
         [ #f (loop)])))))
      
(module+ test
    (define (rs-t-test)
      (let* ([event1
              (lambda (step-time)
                (printf "Event 1 is called with step time ~a\n" step-time))]
             [event2
              (lambda (step-time)
                (printf "Event 2 is called with step time ~a\n" step-time))]
             [sequence1
              (list '() event1 '() event1 '())]
             [sequence2
              (list '() event2 '() event2 '() event2)]
             [track
              (rs-t-create #:bpm 128 #:seq sequence1)])
        (define track-thread (rs-t-play track))
        (sleep 4)
        (set-rs-t-seq! track sequence2)

        (sleep 4)
        (thread-send track-thread
                     'stop)))
  (rs-t-test)
  )
