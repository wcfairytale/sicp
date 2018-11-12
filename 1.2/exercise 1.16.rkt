#lang racket

(define (square x) (* x x))
(define (is_even n) (= (remainder n 2) 0))

;when n is even, b^n = (b^2)^(n/2)
(define (fast_expt b n)
  (cond ((= n 0) 1)
        ((is_even n) (square (fast_expt b (/ n 2))))
        (else (* b (fast_expt b (- n 1))))))

(define (fast_expt2 b n a)
  (cond ((= n 0) a)
        ((is_even n) (fast_expt2 (square b) (/ n 2) a))
        (else (fast_expt2 b (- n 1) (* b a)))))
  
(define (expt b n)
  (fast_expt2 b n 1))


  
      