#lang racket

;; Code for dealing with events.

(require "rs-util.rkt")

(provide (struct-out rs-e)
         rs-e-create)

(struct rs-e (fn offset) #:mutable #:transparent)

; There is probably another way to check if something is null or a
; procedure in a contract but I couldn't (quickly) figure out how. So
; use a helper function.
(define (procedure-or-null? input)
  (or (procedure? input) (null? input)))

; Make sure no nonsensical ranges can be supplied.
(define (offset-valid? input)
  (and (real? input)
       (> input -1)
       (< input 1)))

(define/contract (rs-e-create #:fn fn #:offset [offset 0])
  ; Create an event struct with a function to run (or null) and an
  ; optional offset (between -1 and +1)
  (->* (#:fn procedure-or-null?)
        (#:offset offset-valid?)
       rs-e?)
  (rs-e fn offset))
