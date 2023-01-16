#lang racket

(require graph)

(define (calc-expectations players)
  (define temp (weighted-graph/directed '()))
  (for ([player players])
    (define rivals (get-neighbors players player))
    (for ([rival rivals])
      ; Only add names to reduce space usage
      (let ([pname (get-field name player)]
            [rname (get-field name rival)])
        (add-directed-edge! temp pname rname (E player rival)))))
  (temp))

(define (g player)
  (let ([phi (get-field phi player)])
    ((/ 1 (sqrt (+ 1 (* 3 (expt (/ phi pi) 2))))))))

(define (exp2 x)
  (* x x))

;Check
(define (E p1 p2)
  (let ([miu1 (get-field miu p1)]
        [miu2 (get-field miu p2)]
        [g2 (g p2)])
    (/ 1 (+ 1 (exp (* (- g2)) (- miu1 miu2))))))

(define (phi-star phi vol-prime)
  (sqrt (+ (exp2 phi) (exp2 vol-prime))))

(define (phi-prime player vol-prime v)
  (/ 1 (sqrt (+ (/ 1 (exp2 (phi-star (get-field phi player) vol-prime)) (/ 1 v))))))

(define (miu-prime player phi-prime performance)
  (+ (get-field miu player) (* (exp2 phi-prime) performance)))

(define (lower-bound f a tau)
  (+ 1 2))

(define (optimal-value f A B eps)
  (define fA (f A))
  (define fB (f B))
  (exp (/ A 2)))

(provide calc-expectations g exp2 lower-bound optimal-value miu-prime phi-prime)