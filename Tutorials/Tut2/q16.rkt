#lang racket
(define (reverse l)
  (foldl cons '() l))