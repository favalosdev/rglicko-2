#lang racket

(require graph racket/trace "player.rkt" "scenario.rkt" "stat.rkt")

(define system%
  (class object%
    (init init-scenario init-tau init-eps)

    (define scenario init-scenario)
    (define tau init-tau)
    (define eps init-eps)

    (define/private (diff-outcome player rival)
      (let ([e (E  player rival)]
            [s (send scenario get-score player rival)]
            [g (get-field g rival)])
        (* g (- s e))))

    (define (performance player rivals)
                  (let ([cdiff (lambda (rival) (diff-outcome player rival))])
                    (for/sum ([rival rivals])
                      (cdiff rival))))

    (define (V player rivals)
                  (/ 1 (for/sum ([rival rivals])
                         (let ([e (E player rival)]
                               [gs (squared (get-field g rival))])
                           (* gs (* e (- 1 e)))))))

    (define (F phis v deltas a x)
      (let* ([taus (squared tau)]
             [ex (exp x)]
             [f1 (apply - (list deltas phis v ex))]
             [f2 (squared (apply + (list phis v ex)))])
        (-
         (/ (* ex f1) (* 2 f2))
         (/ (- x a) taus))))

    (define (vol-prime player v delta)
      (define phi (get-field phi player))
      (define vol (get-field vol player))
      (define phis (squared phi))
      (define deltas (squared delta))
      (define a (log (squared vol)))
      (define f (lambda (x) (F phis v deltas a x)))
      (define A a)
      (define B (if (> deltas (+ phis v))
                    (log (apply - (list deltas phis v)))
                    (lower-bound f a tau)))
      (optimal-value f A B eps))

    (define (update-rating player)
      (define rivals (send scenario get-rivals player))

      ; Step 3
      (define v (V player rivals))
      (define current-performance (performance player rivals))

      ; Step 4
      (define delta (* v current-performance))

      ; Step 5
      (define volp (vol-prime player v delta))

      ; Step 6 & 7
      (define phip (phi-prime player volp v))
      (define miup (miu-prime player phip current-performance))

      ; Step 8
      ; Finally, return something new
      (define updated-player (new player%
                                  [name (get-field name player)]
                                  [r (miu->r miup)]
                                  [rd (phi->rd phip)]
                                  [vol volp]))
      updated-player)

    (define/public (run)
      (for/list ([player (send scenario get-players)])
        (update-rating player)))

    (super-new)))

(provide system%)