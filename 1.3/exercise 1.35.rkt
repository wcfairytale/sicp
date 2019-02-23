#lang racket

(define tolerance 0.00001)

(define (average x y) (/ (+ x y) 2.0))
(define (positive x) (> x 0))
(define (negative x) (< x 0))

(define (close_enough x y) (< (abs (- x y)) tolerance))

;fixed-point
(define (fixed_point f first_guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close_enough guess next)
          next
          (try next))))
  (try first_guess))

;golden-ratio
(define (golden_ratio)
  (fixed_point (lambda (x) (+ 1 (/ 1 x)))
               1.0))