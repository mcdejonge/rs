#lang racket

;; This demo shows how you can sequence drums. It uses the stock
;; Ableton 808 kit on channel 1 of MIDI port 0.
;; 
;; An Ableton drum kit has notes assigned to each sample (see
;; below). A useful way to work with these kits is to assign events to
;; each of these notes and give them meaningful names.
;;
;; This demo also makes use of the new nested sequences feature.

(require "../rs.rkt"
         "../rs-m.rkt"
         "../rs-util.rkt")

(when (not (length (list)))
  (printf "No MIDI ports available. This will not work.\n"))

(rs-set-global-bpm! 128)
(rs-set-global-div-length! 1/4)
(rs-set-global-steps! 16)

(rs-start-main-loop!)

;; Uncomment if you want diagnostics. This is awful for performance.
;; (rs-util-set-diag-mode #t)

;; The kit is on channel 1 of MIDI port 0
(define kit (rs-m-instr 0 1))

;; Drum samples map to notes.
(define bd (rs-m-event-play kit 36 25 127))
(define rm (rs-m-event-play kit 37 25 127))
(define sd (rs-m-event-play kit 38 25 127))
(define cp (rs-m-event-play kit 39 25 127))
(define cv (rs-m-event-play kit 40 25 127))
(define tl (rs-m-event-play kit 41 25 127))
(define hc (rs-m-event-play kit 42 25 127))
(define tm (rs-m-event-play kit 43 25 127))
(define mc (rs-m-event-play kit 44 25 127))
(define th (rs-m-event-play kit 45 25 127))
(define ho (rs-m-event-play kit 46 25 127))
(define cl (rs-m-event-play kit 47 25 127))
(define cm (rs-m-event-play kit 48 25 127))
(define cy (rs-m-event-play kit 49 25 127))
(define ch (rs-m-event-play kit 50 25 127))
(define cb (rs-m-event-play kit 51 25 127))

;; To test a sound, uncomment the line below and set the desired sound. 
;((rs-e-fn rm) 100)

;; Some sequences to start with.

;; Electro style.
(define seq1 (list
              bd
              null
              null

              sd
              null
              null
              ))
;; To run a sequence, create a track that uses it and queue it.
(define track1 (rs-track seq1))
(rs-queue-track! track1)

(sleep 4)

;; Add hi hats.
(define hh (list
            mc
            hc
            mc
            hc
            mc
            hc
            mc
            ho
            ))
(define track2 (rs-track hh ))
(rs-queue-track! track2)

(sleep 4)
;; Let's change the sequence for track 1
(set-rs-t-seq! track1 (list
                       bd
                       bd
                       null
                       null
                       null
                       null
                       sd
                       null
                       ))

(sleep 4)
;; Back to the original sequence for track 1
(set-rs-t-seq! track1 seq1)

(sleep 4)
;; Stop the hihats
(rs-stop-track! 1)

(sleep 4)
;; Bring them back
(rs-queue-track! track2)

(sleep 4)
;; Make them fancier. Lists in lists!
(set-rs-t-seq! track2 (list
                       mc
                       hc
                       mc
                       (list hc hc)
                       mc
                       hc
                       mc
                       (list hc hc hc)
                       ))

(sleep 4)

;; Back to regular hi-hats.
(set-rs-t-seq! track2 hh)

(sleep 4)

;; Done
(rs-stop-main-loop!)
