#lang racket

;procedure pair
(define (cons x y)
  (lambda (m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1" m)))))
(define (car z) (z 0))
(define (cdr z) (z 1))

(define test_pair (cons 2 5))

;test case
;(car test_pair)
;2
;(cdr test_pair)
;5