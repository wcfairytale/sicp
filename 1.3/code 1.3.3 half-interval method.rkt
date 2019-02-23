#lang racket

(define (average x y) (/ (+ x y) 2.0))
(define (positive x) (> x 0))
(define (negative x) (< x 0))

(define (close_enough x y) (< (abs (- x y)) 0.001))

;search
(define (search f neg_point pos_point)
  (let ((mid_point (average neg_point pos_point)))
    (if (close_enough neg_point pos_point)
        mid_point
        (let ((test_value (f mid_point)))
          (cond ((positive test_value)
                 (search f neg_point mid_point))
                ((negative test_value)
                 (search f mid_point pos_point))
                (else mid_point))))))

;half-interval-method
(define (half_interval_method f a b)
  (let ((a_value (f a))
        (b_value (f b)))
    (cond ((and (negative a_value) (positive b_value))
           (search f a b))
          ((and (negative b_value) (positive a_value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))