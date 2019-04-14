#lang racket

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

(define test_pair (cons 2 5))

;test case
;(car test_pair)
;2
;(cdr test_pair)
;5