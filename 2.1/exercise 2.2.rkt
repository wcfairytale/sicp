#lang racket

(define (average x y) (/ (+ x y) 2.0))

;point
(define (make_point x y) (cons x y))
(define (point_x p) (car p))
(define (point_y p) (cdr p))
(define (point_display p)
  (display "(")
  (display (point_x p))
  (display ",")
  (display (point_y p))
  (display ")"))

;segment
(define (make_segment x y) (cons x y))
(define (segment_start x) (car x))
(define (segment_end x) (cdr x))

(define (segment_midpoint x)
  (make_point (average (point_x (segment_start x))
                       (point_x (segment_end x)))
              (average (point_y (segment_start x))
                       (point_y (segment_end x)))))
              
;test case
;(point_display (segment_midpoint (make_segment (make_point 1 1) (make_point 5 5))))
;should display (3,3)