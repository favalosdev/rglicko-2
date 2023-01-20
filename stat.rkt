#lang racket

(define (squared x)
  (* x x))

; Convert to scale
(define (r->miu r)
  (/ (- r 1500) 173.7178))

(define (rd->phi rd)
  (/ rd 173.7178))

(define (G phi)
  (/ 1 (sqrt (+ 1 (* 3 (squared (/ phi pi)))))))

; Return from scale

(define (miu->r miu)
  (+ 1500 (* miu 173.7178)))

(define (phi->rd rd)
  (* 173.7178 rd))

;Check
(define (E p1 p2)
  (let ([miu1 (get-field miu p1)]
        [miu2 (get-field miu p2)]
        [g2 (get-field g p2)])
    (/ 1 (+ 1 (exp (* (- g2) (- miu1 miu2)))))))

(define (phi-star phi vol-prime)
  (sqrt (+ (squared phi) (squared vol-prime))))

(define (phi-prime player vol-prime v)
  (/ 1 (sqrt (+ (/ 1 (squared (phi-star (get-field phi player) vol-prime)) (/ 1 v))))))

(define (miu-prime player phi-prime performance)
  (+ (get-field miu player) (* (squared phi-prime) performance)))

(define (lower-bound f a tau)
  (define (loop k)
    (if (< (f (- a (* k tau))) 0)
        (loop (+ k 1))
        (- a (* k tau))))
  (loop 1))

(define (optimal-value f A B eps)
  (define (loop X Y fX fY)
    (if (> (abs (- Y X)) eps)
        (let* ([Z (+ X (/ (* (- X Y) fX) (- fY fX)))]
               [fZ (f Z)])
          (if (<= (* fZ fY) 0)
              (loop Y Z fY fZ)
              (loop X Z (/ fX 2) fZ)))
        (exp (/ X 2))))
  (loop A B (f A) (f B)))

(provide squared r->miu miu->r rd->phi phi->rd G E phi-prime miu-prime lower-bound optimal-value)