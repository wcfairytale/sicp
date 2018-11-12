#lang racket

(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (is_even x) (= (remainder x 2) 0))

;O(n)
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

;O(logn)
(define (mul a b)
  (cond ((= b 0) 0)
        ((is_even b) (mul (double a) (halve b)))
        (else (+ a (mul a (- b 1))))))
      