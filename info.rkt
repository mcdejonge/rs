;; info.rkt
#lang info
(define collection "rs")
(define deps '("base"
               "rackunit"
               "rtmidi"))
(define scribblings '(("scribblings/rs.scrbl" )))
(define build-deps '("scribble-lib" "racket-doc"))
