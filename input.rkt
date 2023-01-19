#lang racket

; This file provides management for CSV and JSON files

(require csv-reading)
(require "glicko-2.rkt")

(define player-csv-reader
  (make-csv-reader-maker
   '((separator-chars            #\|)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define results-csv-reader
  (make-csv-reader-maker
   '((separator-chars             #\|)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define raw-players
  (csv->list (player-csv-reader (open-input-file "data/players.csv"))))

(define raw-results
  (csv->list (results-csv-reader (open-input-file "data/results.csv"))))

(define updated-players (glicko-2 raw-players raw-results 0.5 0.000001))

(for ([player updated-players])
  (display player))