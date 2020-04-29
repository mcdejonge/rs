#lang racket

;; Code for dealing with tracks.

(require "rs-util.rkt"
         "rs-e.rkt")

(provide (struct-out rs-t)
         rs-t-create
         rs-t-play!
         rs-t-valid-sequence?)

(struct rs-t (bpm
              steps
              div-length
              seq)
  #:mutable #:transparent) 

(define (rs-t-valid-sequence? input)
  ; Utility function that validates a sequence. This is provided to
  ; modules calling this module.
  (and (list? input)
       (event-or-null? (car input))
       (or (null? (cdr input))
           (rs-t-valid-sequence? (cdr input)))))


; Int, int, positive -> rs-t
(define/contract (rs-t-create #:bpm bpm
                     #:steps [steps 16]
                     #:div-length [div-length 1/4]
                     #:seq [seq '()])
  ; Create a new track.
  (->* (#:bpm positive?)
      (#:steps positive?
       #:div-length positive?
       #:seq rs-t-valid-sequence?)
      rs-t?)
  (rs-t bpm steps div-length seq))


(define/contract (rs-t-calculate-loop-length track)
  ; Return the length of a single loop of the given track in ms.
  (-> rs-t? any)
  (let* ([beat-length-ms (/ 60000 (rs-t-bpm track))]
         [div-length-ms (* beat-length-ms (rs-t-div-length track))])
    (* (rs-t-steps track) div-length-ms)))

(define/contract (rs-t-play-single-loop! track loop-length)
  ; Play a single iteration of the current seq for the track.
  ; TODO deal with offsets
  (-> rs-t? positive? void)
  (let ([div-length-ms (- (/ loop-length (length (rs-t-seq track))) 0)])
    ;; (for ([seq-item (rs-t-seq track)])
    ;;   (when (rs-e? seq-item)
    ;;     (when (procedure? (rs-e-fn seq-item))
    ;;       (thread (lambda () ((rs-e-fn seq-item) div-length-ms)))))
    ;;   (rs-util-rtsleep-measure (inexact->exact (round div-length-ms)) 200)
    ;;    )
    (for/fold
        ([current-step-length (truncate div-length-ms)]
         [expect-end-at (+ (truncate (current-inexact-milliseconds)) div-length-ms)])
        ([seq-item (rs-t-seq track)])
      (when (rs-e? seq-item)
        (when (procedure? (rs-e-fn seq-item))
          (thread (lambda () ((rs-e-fn seq-item) div-length-ms)))))
      (rs-util-rtsleep (inexact->exact (round (exact->inexact current-step-length))) 2)
      (values (+ div-length-ms (- (truncate (current-inexact-milliseconds)) expect-end-at))
              (+ (+ div-length-ms (- (truncate (current-inexact-milliseconds)) expect-end-at))
                 (truncate (current-inexact-milliseconds)
                  )))
      )
    (void)))

(define (event-or-null? input)
  ; Check if something is an event (see rs-e) or null.
  (or (rs-e? input) (null? input)))

(define (rs-t-next-loop-length correct-loop-length prev-loop-length expected-end)
  ; Calculate the length of a loop taking into account how long it
  ; took the last one to complete vs how long we expected it to take.
  ; Take into account the correct loop length. The new loop length
  ; should be no less than 90% of the correct loop length and no more
  ; than 110% of the correct loop length.  Returns a list containing
  ; the correct loop length, a new loop length and a new end time and
  ; the difference.
  (let* ([now (truncate (current-inexact-milliseconds))]
         [difference (- now expected-end)]
         [min-loop-length (* (/ 9 10) correct-loop-length)]
         [max-loop-length (* (/ 11 10) correct-loop-length)]
         [next-loop-length (- prev-loop-length difference)]
         [real-next-loop-length
          (max min-loop-length (min next-loop-length max-loop-length))]
         [next-loop-end (+ now next-loop-length)])
    (list real-next-loop-length next-loop-end difference)))
  
(define (rs-t-play! track)
  (-> rs-t? thread?)
  ; Return a thread that plays continuously until it receives a 'stop message.
  (thread
   (lambda ()
     (let ([loop-length (rs-t-calculate-loop-length track)])
       (let loop([next-loop-length loop-length]
                 [expect-end-at (+ (current-inexact-milliseconds) loop-length )]
                 [difference 0])
         (collect-garbage 'minor)
         (thread (lambda ()
                   (rs-t-play-single-loop! track (rs-t-calculate-loop-length track))
                   ))
         (rs-util-rtsleep (rs-t-calculate-loop-length track) 2)
         (match (thread-try-receive)
           ; If all you want to do is change the sequence, you do not
           ; need to send a new track as the new sequence is picked up
           ; automatically. You only need this if you want to replace
           ; the currently running track with another.
           [(? rs-t? new-track-info)
            (set! track new-track-info)
            (apply loop (rs-t-next-loop-length loop-length next-loop-length expect-end-at))]
           [ 'stop
             (void)]
           [ #f
            (apply loop (rs-t-next-loop-length loop-length next-loop-length expect-end-at))]
                      ))))))
       
(module+ test
  (define (rs-t-test)
    (let* ([event1
            (rs-e-create
             #:fn (lambda (step-time)
                    (printf "Event 1 is called with step time ~a\n" step-time)))]
           [event2
            (rs-e-create
             #:fn (lambda (step-time)
                    (printf "Event 2 is called with step time ~a\n" step-time))
             #:offset 1/4)]
           [sequence1
            (list '() event1 '() event1 '())]
           [sequence2
            (list '() event2 '() event2 '() event2)]
           [track
            (rs-t-create #:bpm 128 #:seq sequence1)])
      (define track-thread (rs-t-play! track))
      (sleep 4)
      (set-rs-t-seq! track sequence2)

      (sleep 4)
      (thread-send track-thread
                   'stop)))
  (rs-t-test)
  )
