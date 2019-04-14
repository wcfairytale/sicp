#lang racket

;interval
(define (make_interval x y)
  (cons x y))
(define (lower_bound interval)
  (car interval))
(define (upper_bound interval)
  (cdr interval))

;helpers
(define (span_zero interval)
  (and (<= (lower_bound interval) 0)
       (>= (upper_bound interval) 0)))

;arithmatics
(define (add_interval a b)
  (make_interval (+ (lower_bound a) (lower_bound b))
                 (+ (upper_bound a) (upper_bound b))))
(define (sub_interval a b)
  (add_interval a
                (make_interval (- upper_bound b)
                               (- lower_bound b))))

(define (mul_interval a b)
  (let ((p1 (* (lower_bound a) (lower_bound b)))
        (p2 (* (lower_bound a) (upper_bound b)))
        (p3 (* (upper_bound a) (lower_bound b)))
        (p4 (* (upper_bound a) (upper_bound b))))
    (make_interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div_interval a b)
  (if (span_zero b)
      (print "error divide by an interval that spans zero")
      (mul_interval a
                    (make_interval (/ 1.0 (upper_bound b))
                                   (/ 1.0 (lower_bound b))))))

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
(define interval_b (make_interval -2 4))

;(print (div_interval interval_a interval_b))