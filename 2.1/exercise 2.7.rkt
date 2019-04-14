#lang racket

;interval
(define (make_interval x y)
  (cons x y))
(define (lower_bound interval)
  (car interval))
(define (upper_bound interval)
  (cdr interval))

;test
(define test_interval (make_interval 2.0 5.0))

;(lower_bound test_interval)
;2

;(upper_bound test_interval)
;5