#lang racket

;; Utility functions for use in rs

(provide
 rs-calculate-division-length-ms
 )



; Int, num -> float
(define (rs-calculate-division-length-ms bpm division-length)
  ; Calculate the length of a division in ms.
  (* (* (/ 60000 bpm) division-length) 1.0))

