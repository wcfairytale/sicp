#lang racket

(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (is_even x) (= (remainder x 2) 0))

(define (mul_iter a b result)
  (cond ((= b 0) result)
        ((is_even b) (mul_iter (double a) (halve b) result))
        (else (mul_iter (double a) (halve (- b 1)) (+ a result)))))
  
(define (russian_peasant a b)
  (mul_iter a b 0))
  