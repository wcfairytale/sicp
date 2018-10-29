#lang racket

(define (min a b) (if (< a b) a b))
(define (square a) (* a a ))

(define (sum_bigger_2_square a b c)
  (- (+ (square a) (square b) (square c)) (square (min (min a b) c))))
