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


;; Subtype of rs-e that has a field for the duration in ms.
(struct rs-t-e-dur rs-e (duration) #:mutable)

(define/contract (add-duration-to-seq seq step-time-ms)
  (-> list? positive? list?)
  ;; Turn a list of rs-e events into a list of rs-t-e-dur events,
  ;; setting the duration to the given step time.
  ;;
  ;; If an event is null, it is turned into an event with an empty function
  ;; If an event is a list (sub sequence) it is embedded in an rs-t-e-dur event.
  (map (lambda (item)
         (cond [(and (list? item)
                     (not (null? item)))
                (rs-t-e-dur item
                            0
                            step-time-ms)]
               
               [(rs-e? item) (rs-t-e-dur (rs-e-fn item)
                                         (rs-e-offset item)
                                         step-time-ms)]
               [else (rs-t-e-dur (lambda (arg) void)
                                 0
                                 step-time-ms)
                     ])) seq))

(define/contract (process-offsets seq )
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
  ;;
  ;; Some special processing has to happen if the first item has such
  ;; a large offset that it would overlap with the last item (which
  ;; can happen after processing). In that case, set the last item to
  ;; 0.1 the regular length of a step and set the first item to it's
  ;; desired length minus 0.1 the regular length of a step.
  

  ;; Before we start processing this gives us the desired step time.
  (define step-time-ms
    (if (> (length seq) 0)
        (rs-t-e-dur-duration (car seq))
        0
        ))
  
  (define (process-items items)
    (rs-util-diag "Processing sequence items ~s\n" items)
    (cond [(< (length items) 2) items]
          [(= (rs-e-offset (car (cdr items))) 0)
           (rs-util-diag "Second item ~s has no offset.\n" (car (cdr items)))
           (cons (car items) (process-items (cdr items)))]
          [else
           (rs-util-diag "Second item ~s has a non-zero offset.\n" (car (cdr items)))
           (set-rs-t-e-dur-duration! (car items)
                                     (+ (rs-t-e-dur-duration (car items))
                                        (* step-time-ms
                                           (rs-e-offset (car (cdr items))))))
           (rs-util-diag "First item duration is now ~s\n"
                         (rs-t-e-dur-duration (car items)))
           (set-rs-t-e-dur-duration! (car (cdr items))
                                     (- (rs-t-e-dur-duration (car (cdr items)))
                                        (* step-time-ms
                                           (rs-e-offset (car (cdr items))))))
           (rs-util-diag "Second item duration is now ~s\n"
                         (rs-t-e-dur-duration (car (cdr items))))
           (cons (car items) (process-items (cdr items)))]))

  (cond [(= (length seq) 0)
         (rs-util-diag "Sequence is empty. Doing nothing.")
         seq]
        [else
         (rs-util-diag "Sequence has length. Process it (step time: ~s).\n" step-time-ms)
         (define intermediate (process-items seq))
         (rs-util-diag "After processing sequence has item lengths ~s\n"
                       (map rs-t-e-dur-duration intermediate))
         (cond [(= (rs-e-offset (car intermediate)) 0)
                (rs-util-diag "First item does not have an offset. No further action.\n")
                intermediate]
               [(> (rs-e-offset (car intermediate)) 0)
                (rs-util-diag "First item has a positive offset.\n")
                (rs-util-diag "Applying offset ~s to first item\n"
                             (rs-e-offset (car intermediate)))
                (define length-dummy-event (* step-time-ms
                                              (abs (rs-e-offset (car intermediate)))))


                (define new-length-start (- (rs-t-e-dur-duration (car intermediate))
                                            length-dummy-event))
                (rs-util-diag "Inserting dummy event of length ~s before start item with new length ~s\n"
                              length-dummy-event
                              new-length-start)


                (set-rs-t-e-dur-duration! (car intermediate) new-length-start)
                (cons (rs-t-e-dur null 0 length-dummy-event)
                      intermediate)]
               [(< (rs-e-offset (car intermediate)) 0)
                (rs-util-diag "First item has a negative offset.\n")
                (define offset-time (* (abs (rs-e-offset (car intermediate))) step-time-ms))
                (define first-step-time (rs-t-e-dur-duration (car intermediate)))
                (rs-util-diag "Applying offset ~s to first item of length ~s\n"
                              offset-time
                              first-step-time)

                (define new-first-step-time offset-time)
                (define length-dummy-event (- first-step-time offset-time))
                (when (< first-step-time offset-time)
                  (rs-util-diag "Offset is larger than step time.\n")
                  (set! length-dummy-event first-step-time)
                  (set! first-step-time offset-time))
                
                (rs-util-diag "Start event is given length ~s and a dummy event with length ~s is created.\n"
                              new-first-step-time
                              length-dummy-event)

                (define new-length-last-item (- (rs-t-e-dur-duration (last intermediate))
                                                new-first-step-time))
                (rs-util-diag "Last event length is reduced from ~s to ~s\n"
                              (rs-t-e-dur-duration (last intermediate))
                              new-length-last-item)
                (when (< new-length-last-item (* 0.1 step-time-ms))
                  (rs-util-diag "Last event is too short. Setting it to minimum length.\n")
                  (set! new-length-last-item (* 0.1 step-time-ms))
                  (set! new-first-step-time (- new-first-step-time
                                               new-length-last-item))
                  (rs-util-diag "After adjustment last item has length ~s and first item ~s\n"
                                new-length-last-item
                                new-first-step-time))
                (set-rs-t-e-dur-duration! (last intermediate) new-length-last-item)
                                          
                (set-rs-t-e-dur-duration! (car intermediate) new-first-step-time)
                
                (cons (rs-t-e-dur null 0 length-dummy-event)
                      (append (cdr intermediate) (list (car intermediate))))])]))
    
  (define/contract (calc-loop-length track)
  ;; Return the length of a single loop of the given track in ms.
    (-> rs-t? any)
    (define beat-length-ms (/ 60000 (rs-t-bpm track)))
    (define step-length-ms (* beat-length-ms (rs-t-div-length track)))
    (* (rs-t-steps track) step-length-ms))

(define/contract (calc-time-corrected pref-length last-diff [max-diff-ratio 1/20])
  (->* (positive? number?)
       (positive?) positive?)
  ;; Calculate the corrected length of something (a step or a loop)
  ;; taking into account the last time it ran. max-diff-ratio is the
  ;; limit to how much correction will take place (as a ratio of the
  ;; preferred length).
  (define max-diff (* pref-length max-diff-ratio))
  (define min-length (- pref-length max-diff))
  (define max-length (+ pref-length max-diff))
  (max min-length (min (- pref-length last-diff) max-length)))

(define/contract (play-seq! seq loop-length)
  ;; Play a single iteration of a sequence during the given number of
  ;; ms. Not merged with play-track-seq! because we need to be able
  ;; to play nested sequences.
  (-> list? positive? void)
  (define step-length-ms (/ loop-length (length seq)))
  (define seq-playable
    (process-offsets
     (add-duration-to-seq seq step-length-ms)))
  (rs-util-diag "Playing sequence ~s for loop length ~s and step time ~s.\n"
                seq-playable loop-length step-length-ms)
  (for/fold ([last-diff 0])
            ([step seq-playable])
    (define corrected-step-length
      (calc-time-corrected (rs-t-e-dur-duration step) last-diff))
    (rs-util-diag "Playing step ~s for time ~s\n" step corrected-step-length)
    (- (rs-util-run-timed-ms
        (cond [(procedure? (rs-e-fn step))
               (lambda() ((rs-e-fn step) corrected-step-length)
                      (rs-util-rtsleep corrected-step-length))]
              [(and (list? (rs-e-fn step))
                    (> (length (rs-e-fn step)) 0))
               (lambda()
                 (rs-util-diag "Encountered sub sequence of ~s items\n" (length (rs-e-fn step)))
                 (play-seq! (rs-e-fn step) corrected-step-length)
                 (rs-util-rtsleep corrected-step-length))]
              [else (rs-util-rtsleep corrected-step-length)]))
       step-length-ms)))
 
(define/contract (play-track-seq! track loop-length)
  ;; Play a single iteration of the sequence of a track. Not merged
  ;; with play-seq! as play-seq is also used to play nested sequences.
  (-> rs-t? positive? void)
  (play-seq! (rs-t-seq track) loop-length))
    
    

(define (event-or-null? input)
  ; Check if something is an event (see rs-e) or null.
  (or (rs-e? input) (null? input)))

(define (rs-t-play! track)
  (-> rs-t? thread?)
  ; Return a thread that plays continuously until it receives a 'stop message.
  (rs-util-diag "Creating a new thread for playing thread ~s\n" track)
  (thread
   (lambda ()
     (let loop ([last-diff 0])
       ;; It's possible the loop length has changed since the last
       ;; time we checked it, so update it each iteration.
       (define loop-length (calc-loop-length track))
       (define corrected-loop-length
         (calc-time-corrected loop-length last-diff))
       (rs-util-diag "Starting new iteration of track loop of ~s ms for ~s ms (diff: ~s)\n"
                     loop-length
                     corrected-loop-length
                     last-diff)
       (define fn-to-run
         (lambda ()
           (collect-garbage 'minor)
           (play-track-seq! track corrected-loop-length)))
       
       (match (thread-try-receive)
         ;; Send a track struct if you want to update *all* the track
         ;; settings, not just the sequence (for that you can just
         ;; change the sequence of the currently running thread).
         [(? rs-t? new-track-info)
          (set! track new-track-info)
          (loop (- (rs-util-run-timed-ms fn-to-run) loop-length))]
         [ 'stop #f]
         [ #f (loop (- (rs-util-run-timed-ms fn-to-run) loop-length))])))))


  
(module+ test
  (require rackunit)

  (define (rs-t-test-calc-loop-length)
    (define track (rs-t-create #:bpm 120
                               #:steps 4
                               #:div-length 1/4
                               #:seq (list '() '() '() '())))
    (check-equal? (calc-loop-length track) 500))
  (rs-t-test-calc-loop-length)
  
  (rs-util-set-diag-mode #f)

  (define (rs-t-test-add-duration)
    ;; Tests for the add-duration-to-seq function.
    (define processed (add-duration-to-seq
                       (list '()
                             (rs-e-create #:fn (lambda (arg) void)))
                       100))
    (define proc-null (car processed))
    (define proc-fn (car (cdr processed)))

    (check-equal? (length processed) 2
                  "The length of a processed sequence is incorrect.")
    
    (check-true (rs-t-e-dur? proc-null)
                "A null 'event' is not turned into an event.")
    (check-equal? (rs-e-offset proc-null) 0
                  "The offset of a null 'event' is not 0.")
    (check-true (procedure? (rs-e-fn proc-null))
                "A null 'event' does not become a function.")
    (check-equal? (procedure-arity (rs-e-fn proc-null)) 1
                  "The function of a null 'event' does not have arity 1.")

    (check-true (rs-t-e-dur? proc-fn)
                "A non-null event is not turned into an event with duration.")
    
    (check-equal? (rs-t-e-dur-duration proc-fn) 100
                  "A (correct) duration is not added."))
  (rs-t-test-add-duration)

  (define (rs-t-test-process-offsets)
    ;; Tests for the process-offsets function.

    (define e-none (rs-e-create #:fn (lambda (x) void)))
    (define e-neg (rs-e-create #:fn (lambda (x) void) #:offset -1/4))
    (define e-pos (rs-e-create #:fn (lambda (x) void) #:offset 1/4))
    
    ;; Test sequences without offsets.
    (define seq-no-offset
      (process-offsets
       (add-duration-to-seq (list '() e-none '() e-none) 100)))
    
    (check-true (and (= (rs-t-e-dur-duration (car seq-no-offset)) 100)
                     (= (rs-t-e-dur-duration (car (cdr seq-no-offset))) 100)
                     (= (rs-t-e-dur-duration (car (cdr (cdr seq-no-offset)))) 100)
                     (= (rs-t-e-dur-duration (car (cdr (cdr (cdr seq-no-offset)))))100))
                "A sequence without offsets does not have correct durations.")

    (define (validate seq lengths step-time-ms msg)
      ;; Helper function for testing the outcomes of various configurations.
      (check-equal?
       (map rs-t-e-dur-duration
            (process-offsets (add-duration-to-seq seq step-time-ms)))
       lengths msg))

    (validate (list e-none e-neg e-neg e-none)
              (list 75 100 125 100)
              100
              "A sequence with negative offsets in the middle produces incorrect results.")
    
    (validate (list e-none e-pos e-pos e-none)
              (list 125 100 75 100)
              100
              "A sequence with positive offsets in the middle produces incorrect results.")

    (validate (list e-none e-neg e-pos e-neg)
              (list 75 150 50 125)
              100
              "A sequence alternating negative and positive offsets in the middle produces incorrect results.")

    (validate (list e-pos e-none e-none)
              (list 25 75 100 100)
              100
              "A sequence starting with a positive offset produces incorrect results.")

    (validate (list e-neg e-none e-none)
              (list 75 100 75 25)
              100
              "A sequence starting with a negative offset produces incorrect results.")

    (validate (list e-pos e-neg e-pos e-neg)
              (list 25 50 150 50 125)
              100
              "A sequence alternating positive and negative offsets produces incorrect results.")

    (validate (list e-neg e-pos e-neg e-pos)
              (list 100 50 150 50 25)
              100
              "A sequence alternating negative and positive offsets produces incorrect results.")

    (define e-neg2 (rs-e-create #:fn (lambda (x) void) #:offset -0.99))
    (validate (list e-neg e-neg2 e-pos)
              (list 1.0 224.0 50 25)
              100
              "A sequence where the first item is shorter than its negative offset produces incorrect results.")
    ;;(rs-util-set-diag-mode #t)
    (validate (list e-neg2 e-neg2 e-pos)
              (list 1.0 224.0 10.0 89.0)
              100
              "A sequence where the first offset would result in overwriting the last item produces incorrect results.")
    ;;(rs-util-set-diag-mode #f)

    (validate (list e-none e-pos e-neg e-pos)
              (list 375  150 450 225)
              300
              "A sequence with a different step length is processed incorrectly.")
    (validate '()
              '()
              100
              "Processing an empty sequence does nothing.")
    )
  

  
  (rs-t-test-process-offsets)



  

  (define (rs-t-test)
    ;; To check if both events have run more than once, create two
    ;; variables and let the two events increase their values. If both
    ;; end up having a value > 1, the two events have taken place more
    ;; than once. Best we can do, I'm afraid.
    ;;
    ;; This test is not fool proof. More specifically it is not thread
    ;; safe. If for some reason the system does not get around to
    ;; executing both events, you get a false negative.
    (define val-event1  0)
    (define val-event2  0)
    
    (let* ([event1
            (rs-e-create
             #:fn (lambda (step-time)
                    (set! val-event1 (+ val-event1 1))))]
           [event2
            (rs-e-create
             #:fn (lambda (step-time)
                    ;;(printf "Executing val-event2\n")
                    (set! val-event2 (+ val-event2 1)))
             #:offset 1/4)]
           [sequence1
            (list '() event1 '() event1 '())]
           [sequence2
            (list '() event2 '() event2 '() event2)]
           [track
            (rs-t-create #:bpm 120 #:steps 4 #:seq sequence1)])
      (define track-thread (rs-t-play! track))
      (sleep 1)
      (set-rs-t-seq! track sequence2)
      (sleep 3)
      (thread-send track-thread
                   'stop)
      ;; (printf "val-event1 ~s and val-event2 ~s\n"
              ;; val-event1
              ;; val-event2)
      (check-true (and (> val-event1 1)
                       (> val-event2 1))
                  "Both sequenced events did not run more than once.")))
  ;; TODO
   (rs-util-set-diag-mode #t)
   (rs-t-test)
  
  )
