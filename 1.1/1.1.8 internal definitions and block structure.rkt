#lang racket
(define (abs x) (if (< x 0) (- x) x))
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  (define (good_enough guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt_iter guess x)
    (if (good_enough guess x)
        guess
        (sqrt_iter (improve guess x) x)))
  (sqrt_iter 1.0 x))

(define (sqrt2 x)
  (define (good_enough guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt_iter guess)
    (if (good_enough guess)
        guess
        (sqrt_iter (improve guess))))
  (sqrt_iter 1.0))



  