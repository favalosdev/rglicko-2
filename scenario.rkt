#lang racket

(require graph "player.rkt")

(define scenario%
  (class object%
    (init raw-players raw-results)

    (define players (build-players raw-players))
    (define results (build-results raw-results))

    (define/private (build-hashes)
      (define h (make-hash))
      (for ([player players])
        (hash-set! h (get-field name player) player))
      h)

    (define hashes (build-hashes))

    (define/private (get-player name)
      (hash-ref hashes name))

    (define/public (get-players) players)

    (define/public (get-rivals player)
      (let* ([pname (get-field name player)]
             [rnames (get-neighbors results pname)])
        (for/list ([rname rnames])
          (get-player rname))))

    (define/public (get-score p1 p2)
      (let ([pname1 (get-field name p1)]
            [pname2 (get-field name p2)])
        (edge-weight results pname1 pname2)))

    (super-new)))

; Auxiliary methods
(define (build-players raw-players)
  (for/list ([info raw-players])
    (let* ([name (list-ref info 0)]
           [r (string->number (list-ref info 1))]
           [rd (string->number (list-ref info 2))]
           [vol (string->number (list-ref info 3))])
      (new player%
           [name name]
           [r r]
           [rd rd]
           [vol vol]))))

(define (build-results raw-results)
  (define g (weighted-graph/directed '()))
  (for ([raw raw-results])
    (let* ([pname1 (list-ref raw 0)]
           [pname2 (list-ref raw 1)]
           [outcome (list-ref raw 2)]
           [weight (cond [(string=? outcome "W") 1.0]
                         [(string=? outcome "D") 0.5]
                         [(string=? outcome "L") 0.0])])
      (add-directed-edge! g pname1 pname2 weight)
      (add-directed-edge! g pname2 pname1 (- 1 weight))))
  g)

(provide scenario%)