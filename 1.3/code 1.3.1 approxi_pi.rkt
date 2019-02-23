#lang racket

;sum template
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;approximation to pi
(define (approx_pi a b)
  (define (pi_term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi_next x)
    (+ x 4))
  (* (sum pi_term a pi_next b) 8))
