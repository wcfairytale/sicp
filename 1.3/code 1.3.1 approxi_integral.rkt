#lang racket

;sum template
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (identity x) x)

;approximation to integral
(define (integral f a b dx)
  (define (add_dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add_dx b) dx))
