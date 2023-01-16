#lang typed/racket

(define player%
  (class object%
    (init-field
     [r : Number]
     [rd : Number]
     [vol : Number])

    (field [miu (/ (- r 1500) 173.7178)]
           [phi (/ rd 173.7178)])

    #|
    (: calc-miu! (-> Void))
    (define/public (calc-miu!)
      (set! miu (/ (- r 1500) 173.7178)))

    (: calc-phi! (-> Void))
    (define/public (calc-phi!)
      (set! phi (/ rd 173.7178)))
    |#

    (: update-r! (-> Number Void))
    (define/public (update-r! miu-prime)
      (set! miu miu-prime)
      (set! r (+ 1500 (* 173.7178 miu-prime))))

    (: update-rd! (-> Number Void))
    (define/public (update-rd! phi-prime)
      (set! phi phi-prime)
      (set! rd (* 173.7178 phi-prime)))

    (super-new)))

(provide player%)