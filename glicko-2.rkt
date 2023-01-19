#lang racket

(require graph)
(require "system.rkt")
(require "player.rkt")

; Returns a list of the players with updated ratings

(define (glicko-2 raw-players raw-results tau eps)
  (define scenario (build-scenario raw-players raw-results tau eps))
  (send scenario run))

(define (build-scenario raw-players raw-results tau eps)
  (define g (weighted-graph/directed '()))
  (define h (make-hash))
  (add-players! g h raw-players)
  (add-results! g h raw-results)
  (new system%
       [init-players g]
       [init-tau tau]
       [init-eps eps]))

(define (add-players! g h raw-players)
  (for ([info raw-players])
    (let* ([name (list-ref info 0)]
           [r (string->number (list-ref info 1))]
           [rd (string->number (list-ref info 2))]
           [vol (string->number (list-ref info 3))]
           [player (new player%
                        [name name]
                        [r r]
                        [rd rd]
                        [vol vol])])
      (hash-set! h name player)
      (add-vertex! g player))))

; Biggest bottlekneck goes in here
(define (add-results! g h raw-results)
  (for ([raw raw-results])
    (let* ([pname1 (list-ref raw 0)]
           [pname2 (list-ref raw 1)]
           [p1 (hash-ref h pname1)]
           [p2 (hash-ref h pname2)]
           [outcome (list-ref raw 2)]
           [weight (cond [(string=? outcome "W") 1.0]
                         [(string=? outcome "D") 0.5]
                         [(string=? outcome "L") 0.0])])
      (add-edge! g p1 p2 weight)
      (add-edge! g p1 p2 (- 1 weight)))))

(provide glicko-2)