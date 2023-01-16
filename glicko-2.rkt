#lang racket

(require "system.rkt")

; Returns a list of the players with updated ratings
(define (glicko-2 players scores tau eps)
  (define scenario (new system%))
  (send scenario run))