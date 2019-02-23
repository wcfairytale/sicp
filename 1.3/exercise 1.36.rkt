#lang racket

(define tolerance 0.00001)

(define (average x y) (/ (+ x y) 2.0))
(define (positive x) (> x 0))
(define (negative x) (< x 0))

(define (close_enough x y) (< (abs (- x y)) tolerance))

;fixed-point
(define (fixed_point f first_guess)
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close_enough guess next)
          next
          (try next))))
  (try first_guess))

;x^x=1000
(define (search x)
  (fixed_point (lambda (y) (average y (/ (log x) (log y))))
               2.0))

(define (search2 x)
  (fixed_point (lambda (y) (/ (log x) (log y)))
               2.0))