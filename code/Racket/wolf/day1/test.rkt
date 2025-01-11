#!/usr/bin/env racket
#lang racket

(require rackunit)
(require "main.rkt") ;; Import the functions from main.rkt

(define (test)
  (define left '(3 4 2 1 3 3))
  (define right '(4 3 5 3 9 3))

  (check-equal? (distance-between-the-two-lists (sort left <) (sort right <)) 11)
  (check-equal? (similarity-score (sort left <) (sort right <)) 31))

(test) ;; Run tests
