#!/usr/bin/env racket
#lang racket

(provide distance-between-the-two-lists
         similarity-score
         read-the-two-lists)

;; Function to read the pair of lists from stdin
(define (read-the-two-lists)
  (let loop ((left '())
             (right '()))
    (define input (read-line))
    (if (eof-object? input)
        ;; Return the pair of lists
        (cons left right)
        ;; Process the line and recurse
        (let* ((parts (string-split input))
               (first-num (string->number (first parts)))
               (second-num (string->number (second parts))))
          (loop (cons first-num left) (cons second-num right))))))

;; Function to compute the sum of absolute differences
;; To get the right answer here, the two lists must already be sorted.
(define (distance-between-the-two-lists left right)
  (let loop ((left left) (right right) (sum 0))
    (if (null? left)
        ;; Base case: when no more elements, return the accumulated sum
        sum
        ;; Recursive case: compute the difference and add to the sum
        (let ((diff (abs (- (first left) (first right)))))
          (loop (rest left) (rest right) (+ sum diff))))))

;; Function to compute the similarity score
(define (similarity-score left right)
  ;; Step 1: Build a dictionary (hash table) for the counts in the right list
  (define right-counts (make-hash))
  (for ([item right])
    (hash-set! right-counts item (add1 (hash-ref right-counts item 0))))

  ;; Step 2: Compute the sum of products
  (foldl (lambda (item acc)
           (+ acc (* item (hash-ref right-counts item 0))))
         0
         left))

;; Entry point function
(define (main)
  ;; Read the input only in the main program
  (define lists (read-the-two-lists))
  ;; Sort before calling distance-between-the-two-lists
  (define left (sort (car lists) <))
  (define right (sort (cdr lists) <))

  (define distance (distance-between-the-two-lists left right))
  (define similarity (similarity-score left right))
  (printf "Day 1, part 1: the distance between the two lists is ~a\n" distance)
  (printf "Day 1, part 2: the similarity score for the two lists is ~a\n" similarity))

;; Only execute `main` if this file is run directly
(module+ main
  (main))
