#lang racket
(define (abs x) (if (< x 0) (- x) x))
(define (square x) (* x x))

(define (good_enough guess better_guess)
  (< (abs (- (/ guess better_guess) 1.0)) 0.001))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (+ guess guess)) 3))
  
(define (cbrt_iter guess x)
  (if (good_enough guess (improve guess x))
      guess
      (cbrt_iter (improve guess x) x)))