#lang racket

(define (power x n)
  (define (iter result i)
    (if (> i n)
        result
        (iter (* result x) (+ i 1))))
  (iter 1 1))

;(power 2 6)
;64

(define (count_factor n t)
  (define (iter num count)
    (if (= (remainder num t) 1)
        count
        (iter (/ num t) (+ count 1))))
  (iter n 0))

;(count_factor 64 2)
;6


(define (cons a b)
  (* (power 2 a) (power 3 b)))
(define (car z)
  (count_factor z 2))
(define (cdr z)
  (count_factor z 3))

(define test_pair (cons 2 5))

;(car test_pair)
;2
;(cdr test_pair)
;5