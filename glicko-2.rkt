#lang racket

(require "system.rkt")

; Returns a list of the players with updated ratings
(define (glicko-2 players results tau eps)
  (define scenario (new system%
                        [init-players players]
                        [init-results results]
                        [init-tau tau]
                        [init-eps eps]))
  (send scenario run))

(provide glicko-2)