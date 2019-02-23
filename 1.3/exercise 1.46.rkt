#lang racket

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2.0))
(define tolerance 0.00001)

;iterative_improve
(define (iterative_improve good_enough improve)
  (lambda (first_guess)
    (define (iter guess)
      (let ((next (improve guess)))
        (if (good_enough guess next)
          next
          (iter next))))
    (iter first_guess)))

;sqrt
(define (sqrt x)
  (define (good_enough a b)
    (< (abs (- a b)) tolerance))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative_improve good_enough improve) 1.0))

;fixed-point
(define (fixed_point f initial_guess)
  (define (good_enough a b)
    (< (abs (- a b)) tolerance))
  (define (improve guess)
    (f guess))
  ((iterative_improve good_enough improve) initial_guess))
 