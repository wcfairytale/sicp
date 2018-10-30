#lang racket
(define (abs x) (if (< x 0) (- x) x))
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (good_enough guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess x) (average guess (/ x guess)))
  
(define (sqrt_iter guess x)
  (if (good_enough guess x)
      guess
      (sqrt_iter (improve guess x) x)))