#lang racket 

(define player%
  (class* object% (equal<%> printable<%>)
    (init-field name r rd vol)

    (field [miu (/ (- r 1500) 173.7178)]
           [phi (/ rd 173.7178)]) 

    (define/public (update-r! miu-prime)
      (set! miu miu-prime)
      (set! r (+ 1500 (* 173.7178 miu-prime))))

    (define/public (update-rd! phi-prime)
      (set! phi phi-prime)
      (set! rd (* 173.7178 phi-prime)))

    (define (player%->string)
      (format "Name: ~a~n Rating: ~a~n Rating deviation: ~a~n Volatility: ~a~n" name r rd vol))

    (define/public (equal-to? other recur)
      (string=? name (get-field name other)))

    (define/public (equal-hash-code-of hash-code)
      (hash-code (string-downcase name)))

    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code (string-downcase name)))

    (define/public (custom-print port quote-depth)
      (print (player%->string) port quote-depth))

    (define/public (custom-write port)
      (write (player%->string) port))    

    (define/public (custom-display port)
      (display (player%->string) port))

    (super-new)))

(provide player%)