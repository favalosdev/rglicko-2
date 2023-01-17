#lang racket

(require graph)
(require "system.rkt")
(require "player.rkt")

; Returns a list of the players with updated ratings
(define (glicko-2 raw-players-info raw-results tau eps)
  (define scenario (build-scenario raw-players-info raw-results tau eps))
  (send scenario run))

(define (build-scenario raw-players-info raw-results tau eps)
  (define players (build-players-list raw-players-info))
  (define results (build-results-graph raw-results))
  (new system%
       [init-players players]
       [init-results results]
       [init-tau tau]
       [init-eps eps]))

(define (build-players-list raw-players-info)
  (for/list ([info raw-players-info])
    (let ([name (list-ref info 0)]
          [r (list-ref info 1)]
          [rd (list-ref info 2)]
          [vol (list-ref info 3)])
      (new player%
           [name name]
           [r r]
           [rd rd]
           [vol vol]))))

; Research how to do it
(define (build-results-graph raw-results)
  (define g (weighted-graph/undirected '()))
  (for ([raw raw-results])
    (let* ([pname1 (list-ref raw 0)]
           [pname2 (list-ref raw 1)]
           [outcome (list-ref raw 2)]
           [weight (cond [(string=? outcome "W") 1.0]
                         [(string=? outcome "D") 0.5]
                         [(string=? outcome "L") 0.0])])
      (add-edge! pname1 pname2 weight)))
  (g))

(provide glicko-2)