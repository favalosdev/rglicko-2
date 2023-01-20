#lang racket

(require "stat.rkt")

(define player%
  (class* object% (printable<%>)
    (init-field name r rd vol)

    (field [miu (r->miu r)]
           [phi (rd->phi rd)]
           [g (G phi)])

    (define (player%->string)
      (format "Name: ~a~n Rating: ~a~n Rating deviation: ~a~n Volatility: ~a~n" name r rd vol))

    (define/public (custom-print port quote-depth)
      (print (player%->string) port quote-depth))

    (define/public (custom-write port)
      (write (player%->string) port))

    (define/public (custom-display port)
      (display (player%->string) port))

    (super-new)))

(provide player%)