#lang typed/racket

(define player%
  (class object%
    (init-field
     [name : String]
     [r : Number]
     [rd : Number]
     [vol : Number])

    (field [miu (/ (- r 1500) 173.7178)]
           [phi (/ rd 173.7178)]) 

    (: update-r! (-> Number Void))
    (define/public (update-r! miu-prime)
      (set! miu miu-prime)
      (set! r (+ 1500 (* 173.7178 miu-prime))))

    (: update-rd! (-> Number Void))
    (define/public (update-rd! phi-prime)
      (set! phi phi-prime)
      (set! rd (* 173.7178 phi-prime)))

    (define/public (lst-repr)
      (list name r rd vol))

    (super-new)))

(provide player%)