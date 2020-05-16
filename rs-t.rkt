#lang racket/base

;; Code for dealing with tracks.

(require racket/contract/base
         racket/contract/region
         racket/match
         racket/list
         "rs-e.rkt"
         "rs-util.rkt")

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
    (round (* (rs-t-steps track) div-length-ms))))

(define/contract (rs-t-play-seq! seq loop-length)
  ;; Play a single iteration of a sequence during the given number of seconds.
  ;; TODO deal with offsets.
  (-> list? positive? void)
  (let* ([div-length-ms (- (/ loop-length (length seq)) 0)]
        [items-executable
         (map (lambda (item)
                (cond [(rs-e? item)
                       (lambda () ((rs-e-fn item) div-length-ms))]
                      [(and (list? item) (> (length item) 0))
                       (lambda ()
                         (rs-util-diag "Encountered sub sequence of ~s items\n" (lambda ()
                                                                                  (length item)))
                         (rs-t-play-seq! item div-length-ms))]
                      [else (void)])) seq)])
    (rs-util-loop-and-wait items-executable div-length-ms 1/10))
  (void))
  
(define/contract (rs-t-play-single-loop! track loop-length)
  ; Play a single iteration of the main sequence for the track.
  (-> rs-t? positive? void)
  (rs-t-play-seq! (rs-t-seq track) loop-length))
    
    

(define (event-or-null? input)
  ; Check if something is an event (see rs-e) or null.
  (or (rs-e? input) (null? input)))

(define (rs-t-play! track)
  (-> rs-t? thread?)
  ; Return a thread that plays continuously until it receives a 'stop message.
  (rs-util-diag "Creating a new thread for playing thread ~s\n" track)
  (thread
   (lambda ()
     (rs-util-loop-and-wait
      (lambda ()
        (collect-garbage 'minor)
        (thread (lambda ()
                  (rs-t-play-single-loop! track (rs-t-calculate-loop-length track))))
        (match (thread-try-receive)
          ; If all you want to do is change the sequence, you do not
          ; need to send a new track as the new sequence is picked upu
          ; automatically. You only need this if you want to replace
          ; the currently running track with another.
          [(? rs-t? new-track-info)
           (set! track new-track-info)
           #t]
          [ 'stop #f]
          [ #f #t])
        )
      (rs-t-calculate-loop-length track) 1/10))))

;; Subtype of rs-e that has a field for the duration in ms.
(struct rs-t-e-dur rs-e (duration) #:mutable)

(define/contract (rs-t-add-duration-to-seq seq step-time-ms)
  (-> list? positive? list?)
  ;; Turn a list of rs-e events into a list of rs-t-e-dur events,
  ;; setting the duration to the given step time.
  (map (lambda (item)
         (rs-t-e-dur (rs-e-fn item)
                     (rs-e-offset item)
                     step-time-ms)
         ) seq))

(define/contract (rs-t-process-offsets seq )
  (-> list? list?)
  ;; Process the durations of the items in a sequence of events (that
  ;; are already turned into rs-t-e-dur events).

  ;; Process all items, only looking at the second one:
  ;; second offset 0? leave 1 and 2 unchanged.
  ;; otherwise: add offset * time of 2 to
  ;; 1 and subtract offset * time of 2 from 2.
  ;;
  ;; Once that is done, check if the very first item has an offset.
  ;; If this offset is positive, decrease the length and add a new
  ;; dummy event to the start of the sequence.  If this offset is
  ;; negative, decrease the length of the last event, add the original
  ;; first event to the end of the sequence (but give it the length of
  ;; the offset only) and add a new dummy event to the start with the
  ;; original length of the first item minus the offset.
  
  (define (process-items items)
    (cond [(= (rs-e-offset (car (cdr items))) 0)
           (cons (car (items)) (process-items (cdr items)))]
          [else
           (set-rs-t-e-dur-duration! (car items)
                                     (+ (rs-t-e-dur-duration (car items))
                                        (* (rs-t-e-dur-duration (car (cdr items)))
                                           (rs-e-offset (car (cdr items))))))
           (set-rs-t-e-dur-duration! (car (cdr items))
                                     (- (rs-t-e-dur-duration (car (cdr items)))
                                        (* (rs-t-e-dur-duration (car (cdr items)))
                                           (rs-e-offset (car (cdr items))))))
           (cons (car (items)) (process-items (cdr items)))]))

  (cond [(> 1 (length seq)) seq]
        [else 
         (define intermediate (process-items seq))
         (cond [(= (rs-e-offset (car intermediate)) 0) intermediate]
               [(> (rs-e-offset (car intermediate)) 0)
                (define new-length-start (- (rs-t-e-dur-duration (car intermediate))
                                            (* (rs-t-e-dur-duration (car intermediate))
                                               (abs (rs-e-offset (car intermediate))))))
                (define length-dummy-event (- (rs-t-e-dur-duration (car intermediate))
                                              new-length-start))

                (set-rs-t-e-dur-duration! (car intermediate) new-length-start)
                (cons (rs-t-e-dur null 0 length-dummy-event)
                      intermediate)]
               [(< (rs-e-offset (car intermediate)) 0)
                (define new-length-start (- (rs-t-e-dur-duration (car intermediate))
                                            (* (rs-t-e-dur-duration (car intermediate))
                                               (abs (rs-e-offset (car intermediate))))))
                (define length-dummy-event (- (rs-t-e-dur-duration (car intermediate))
                                              new-length-start))

                (set-rs-t-e-dur-duration! (last intermediate)
                                          (- (rs-t-e-dur-duration (last intermediate))
                                             new-length-start))

                (set-rs-t-e-dur-duration! (car intermediate) new-length-start)
                
                (cons (rs-t-e-dur null 0 length-dummy-event)
                      (append (cdr intermediate) (list (car intermediate))))])]))
    
  

  
(module+ test
  (require rackunit)
  (rs-util-set-diag-mode #t)
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
      (sleep 2)
      (set-rs-t-seq! track sequence2)

      (sleep 2)
      (thread-send track-thread
                   'stop)))
  (rs-t-test)
  
  )
