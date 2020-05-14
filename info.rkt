;; info.rkt
#lang info
(define collection "rs")
(define deps '("base"
               "rtmidi"))
(define scribblings '(("scribblings/rs.scrbl" (multi-page))))
(define build-deps '("scribble-lib" "racket-doc"))
