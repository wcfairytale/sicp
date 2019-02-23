#lang racket

;identity
(define (identity x) x)

;add_one
(define (add_one x) (+ x 1))

;accumulate
(define (accumulate combiner null_value term a next b)
  (if (> a b)
      null_value
      (combiner (term a)
                (accumulate combiner null_value term (next a) next b))))

(define (accumulate_iter combiner null_value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null_value))

;sum using accumulate
(define (sum term a next b)
  (accumulate + 0 term (next a) next b))
(define (sum_iter term a next b)
  (accumulate_iter + 0 term (next a) next b))

;product using accumulate
(define (product term a next b)
  (accumulate * 1 term (next a) next b))
(define (product_iter term a next b)
  (accumulate_iter * 1 term (next a) next b))
