#lang racket

(require graph)
(require "system.rkt")
(require "scenario.rkt")

; Returns a list of the players with updated statistics 
(define (glicko-2 raw-players raw-results tau eps)
  (define s (build-system raw-players raw-results tau eps))
  (send s run))

(define (build-system raw-players raw-results tau eps)
  (define s (new scenario%
                 [raw-players raw-players]
                 [raw-results raw-results]))
  (new system%
       [init-scenario s]
       [init-tau tau]
       [init-eps eps]))

(provide glicko-2)