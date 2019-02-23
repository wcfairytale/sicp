#lang racket

(define (one i) 1.0)
(define (di i)
  (if (= (remainder (+ i 1) 3) 0)
      (* (/ (+ i 1) 3) 2)
      1))

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

;approxi e
(define (approxi_e k)
  (+ 2 (cont_frac one di k)))