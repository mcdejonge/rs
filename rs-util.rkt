#lang racket

;; Utility functions for use in rs

(provide
 rs-calculate-div-length-ms
 )



; Int, num -> float
(define (rs-calculate-div-length-ms bpm div-length)
  ; Calculate the length of a division in ms.
  (* (* (/ 60000 bpm) div-length) 1.0))

