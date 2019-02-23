#lang racket

;identity
(define (identity x) x)

;square
(define (square x) (* x x))

;add_one
(define (add_one x) (+ x 1))

;divides
(define (divides a b) (= (remainder b a) 0))

;gcd
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;prime
(define (find_divisor x test_divisor)
  (cond ((> (square test_divisor) x) x)
        ((divides test_divisor x) test_divisor)
        (else (find_divisor x (+ test_divisor 1)))))
(define (min_divisor x) (find_divisor x 2))

(define (prime x) (= x (min_divisor x)))

;relative_prime
(define (relative_prime a b)
  (= (gcd a b) 1))

;accumulate with filter
(define (filtered_accumulate combiner null_value term a next b predicate)
  (if (> a b)
      null_value
      (if (predicate a)
          (combiner (term a) (filtered_accumulate combiner null_value term (next a) next b predicate))
          (filtered_accumulate combiner null_value term (next a) next b predicate))))

;sum using accumulate
(define (filtered_sum term a next b predicate)
  (filtered_accumulate + 0 term (next a) next b predicate))

;product using accumulate
(define (filtered_product term a next b predicate)
  (filtered_accumulate * 1 term (next a) next b predicate))

;product of relative primes
;gcd need 2 params, but the filtered_accumulation's predicate need only 1 param,
;how to solve it, is the tricky part of this problem
(define (product_of_relative_primes n)
  (define (filter x) (relative_prime x n))
  (filtered_accumulate * 1 identity 1 add_one (- n 1) filter))
