#lang racket

(define pi 3.1415926)

;k-term finite continued fraction
(define (cont_frac n d k)
  (define (cf i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cf (+ i 1))))))
  (cf 1))

(define (cont_frac_iter n d k)
  (define (iter result i)
    (if (= i 0)
        result
        (iter (/ (n i) (+ (d i) result)) (- i 1))))
  (iter 0 k))

;approxi tan
(define (tan_cf x k)
  (define (ni i)
    (if (= i 1)
        x
        (- (* x x))))
  (define (di i) (+ 1 (* 2 (- i 1))))
  (cont_frac ni di k))