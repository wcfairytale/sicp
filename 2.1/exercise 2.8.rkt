#lang racket

;interval
(define (make_interval x y)
  (cons x y))
(define (lower_bound interval)
  (car interval))
(define (upper_bound interval)
  (cdr interval))

;arithmatics
(define (sub_interval a b)
  (make_interval (- (lower_bound a) (upper_bound b))
                 (- (upper_bound a) (lower_bound b))))

;test
(define (print_interval interval)
  (display "[")
  (display (lower_bound interval))
  (display ",")
  (display (upper_bound interval))
  (display "]"))

(define interval_a (make_interval 6 10))
(define interval_b (make_interval 2 4))
(define test_interval (sub_interval interval_a interval_b))

;(print_interval test_interval)
;[2,8]