#lang racket

(require graph)
(require "calc.rkt")

(define system%
  (class object%
    (init init-players init-results init-tau init-eps)

    (define players init-players)
    (define results init-results)
    (define tau init-tau)
    (define eps init-eps)

    (define expectations (calc-expectations players))

    (define/private (get-expectation p1 p2)
      (let ([name1 (get-field name p1)]
            [name2 (get-field name p2)])
        (edge-weight expectations name1 name2)))

    (define/private (get-score p1 p2)
      (edge-weight results p1 p2))

    (define/private (diff-outcome player rival)
      (let ([E (get-expectation player rival)]
            [s (get-score player rival)])
        (* (g rival) (- s E))))

    (define/private (performance player rivals)
      (let ([cdiff (lambda (rival) (diff-outcome player rival))])
        (for/sum ([rival rivals])
          (cdiff rival))))

    (define/private (V player rivals)
      (/ 1 (for/sum ([rival rivals])
             (let ([E (get-expectation player rival)]
                   [gs (exp2 (g rival))])
               (* gs (* E (- 1 E)))))))

    (define/private (F phis v deltas a x)
      (let* ([taus (exp2 tau)]
             [ex (exp x)]
             [f1 (apply - (list deltas phis v ex))]
             [f2 (exp2 (apply + (list phis v ex)) x)])
        (-
         (/ (* ex f1) (* 2 f2))
         (/ (- x a) taus))))

    (define/private (vol-prime player v delta)
      (define phi (get-field phi player))
      (define vol (get-field vol player))
      (define phis (exp2 phi))
      (define deltas (exp2 delta))
      (define a (log (exp2 vol)))
      (define f (lambda (x) (F phis v deltas a x)))
      (define A a)
      (define B (if (> deltas (+ phis v))
                    (log (apply - (list deltas phis v)))
                    (lower-bound f a tau)))
      (optimal-value f A B eps))

    (define/private (update-rating player)
      (define rivals (get-neighbors player))

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
      ; Finally, update everything
      (send player update-r! miup)
      (send player update-rd! phip)
      (set-field! vol player volp)

      ; Return player class with changed attributes
      (player))

    (define/public (run)
      (for ([player players])
        (update-rating player)))

    (super-new)))

(provide system%)