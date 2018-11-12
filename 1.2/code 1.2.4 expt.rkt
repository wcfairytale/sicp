#lang racket

(define (square x) (* x x))
(define (is_even n) (= (remainder n 2) 0))

;expt_recur O(n)
(define (expt_recur b n)
  (if (= n 0)
      1
      (* b (expt_recur b (- n 1)))))

;expt_iter O(n)
(define (expt b n)
  (define (expt_iter counter product)
    (if (= counter 0)
        product
        (expt_iter (- counter 1) (* b product))))
  (expt_iter n 1))

;fast_expt O(logn)
(define (fast_expt b n)
  (cond ((= n 0) 1)
        ((is_even n) (square (fast_expt b (/ n 2))))
        (else (* b (fast_expt b (- n 1)))))) 
      