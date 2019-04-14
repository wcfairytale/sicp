#lang racket

;interval
(define (make_interval x y)
  (cons x y))
(define (lower_bound interval)
  (car interval))
(define (upper_bound interval)
  (cdr interval))

;arithmatics
(define (add_interval a b)
  (make_interval (+ (lower_bound a) (lower_bound b))
                 (+ (upper_bound a) (upper_bound b))))
(define (sub_interval a b)
  (make_interval (- (lower_bound a) (upper_bound b))
                 (- (upper_bound a) (lower_bound b))))

(define (interval_width interval)
  (/ (- (upper_bound interval)
        (lower_bound interval))
     2))

;test
(define (print_interval interval)
  (display "[")
  (display (lower_bound interval))
  (display ",")
  (display (upper_bound interval))
  (display "]"))

(define interval_a (make_interval 6 10))
(define interval_b (make_interval 2 4))
(define interval_a_add_b (add_interval interval_a interval_b))
(define interval_a_sub_b (sub_interval interval_a interval_b))

;(print interval_a_add_b)
;(print (interval_width interval_a_add_b))
;(print (+ (interval_width interval_a) (interval_width interval_b)))

;(print interval_a_sub_b)
;(print (interval_width interval_a_sub_b))
;(print (+ (interval_width interval_a) (interval_width interval_b)))

;[8,14]
;3
;3

;[2,8]
;3
;3