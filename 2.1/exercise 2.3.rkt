#lang racket

;point
(define (make_point x y) (cons x y))
(define (point_x p) (car p))
(define (point_y p) (cdr p))

;segment
(define (make_segment x y) (cons x y))
(define (segment_start x) (car x))
(define (segment_end x) (cdr x))

;rectangle defined by LeftTopPoint and RightBottomPoint
(define (make_rect lt rb) (cons lt rb))
(define (rect_lt x) (car x))
(define (rect_rb x) (cdr x))
(define (rect_width x)
  (- (point_x (rect_rb x))
     (point_x (rect_lt x))))
(define (rect_height x)
  (- (point_y (rect_lt x))
     (point_y (rect_rb x))))
(define test_rect
  (make_rect (make_point 1 4) (make_point 5 2)))

;rectangle defined by LeftSegment and TopSegment
;(define (make_rect ls ts) (cons ls ts))
;(define (rect_ls x) (car x))
;(define (rect_ts x) (cdr x))
;(define (rect_width x)
;  (- (point_x (segment_end (rect_ts x)))
;     (point_x (segment_start (rect_ts x)))))
;(define (rect_height x)
;  (- (point_y (segment_end (rect_ls x)))
;     (point_y (segment_start (rect_ls x)))))
;(define test_rect
;  (make_rect (make_segment (make_point 1 2)
;                           (make_point 1 4))
;             (make_segment (make_point 1 4)
;                           (make_point 5 4))))
  
;application
(define (rect_perimeter x)
  (* (+ (rect_width x)
        (rect_height x))
     2))
(define (rect_area x)
  (* (rect_width x) (rect_height x)))

;test case
;(rect_perimeter test_rect)
;12
;(rect_area test_rect)
;8
