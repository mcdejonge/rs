#lang racket/base

;; Code for dealing with events.

(require racket/contract/base
         racket/contract/region)

(provide (struct-out rs-e)
         rs-e-create
         rs-e-multiple)

(struct rs-e (fn offset) #:mutable #:transparent)

; There is probably another way to check if something is null, a list or a
; procedure in a contract but I couldn't (quickly) figure out how. So
; use a helper function. TODO extract this sort of stuff into an util file.
(define (procedure-list-or-null? input)
  ; Check if something is a procedure or null.
  (or (procedure? input) (null? input) (list? input)))

(define (list-of-event-procs? input)
  ; Check if something is a list of procedures.
  (and (list? input)
       (and (procedure? (car input)) (= 1 (procedure-arity (car input))))
       (or (null? (cdr input))
           (list-of-event-procs? (cdr input)))))

(define (offset-valid? input)
  ; Make sure no nonsensical ranges can be supplied.
  (and (real? input)
       (> input -1)
       (< input 1)))

(define/contract (rs-e-create #:fn fn #:offset [offset 0])
  ; Create an event struct with a function to run (or null) and an
  ; optional offset (between -1 and +1)
  (->* (#:fn procedure-list-or-null?)
        (#:offset offset-valid?)
       rs-e?)
  (rs-e fn offset))

(define/contract (rs-e-multiple procedures)
  ; Return a procedure that runs all the supplied procedures.
  (-> list-of-event-procs? procedure?)
  (lambda (step-time)
    (for ([procedure procedures])
      (thread (lambda ()
                (procedure step-time) )))))

(module+ test
  (define (rs-e-test)
    (let* ([proc1
            (lambda (step-time)
              (printf "Calling proc 1 with argument ~a\n" step-time))]
           [proc2
            (lambda (step-time)
              (printf "Calling proc 2 with argument ~a\n" step-time))]
           [multi
            (rs-e-multiple (list proc1 proc2))])
      (printf "You should see both proc1 and proc2 being called with argument 3.\n")
      (multi 3)))
  (rs-e-test))
