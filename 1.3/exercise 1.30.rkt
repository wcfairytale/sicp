#lang racket

;identity
(define (identity x) x)

;next
(define (next x) (+ x 1))

;iteractive sum template
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


