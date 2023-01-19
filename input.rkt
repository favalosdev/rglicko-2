#lang racket

; This file provides management for CSV and JSON files

(require csv-reading)
(require "glicko-2.rkt")

(define player-csv-reader
  (make-csv-reader
   '((separators-chars            #\;)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define results-csv-reader
  (make-csv-reader
   '((separator-chars             #\;)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define raw-player-info
  (csv->list (player-csv-reader (open-input-file "players.csv"))))

(define raw-results
  (csv->list (results-csv-reader (open-input-file "results.csv"))))

(define updated-players (glicko-2 raw-player-info raw-results))

(for ([player updated-players])
  (display player))