#lang racket

(define (inc x) (+ x 1))
(define (square x) (* x x))

(define (even x) (= (remainder x 2) 0))

(define (compose f g)
  (lambda (x)
    (f (g x))))

;repeat f n times
(define (repeated f n)
  (if (= n 1)
      f
      (compose f
               (repeated f (- n 1)))))

(define (repeated_iter f n)
  (define (iter i repeated_f)
    (if (= i 1)
        repeated_f
        (iter (- i 1)
              (compose f repeated_f))))
  (iter n f))

;smooth
(define dx 0.000001)

(define (smooth f)
  (lambda (x)
    (/ (+
        (f (- x dx))
        (f x)
        (f (+ x dx)))
       3.0)))

;n-fold smooth
(define (n_fold_smooth f n)
  (repeated smooth n) f)

;n-fold smooth using let
(define (n_fold_smooth2 f n)
  (let (smooth_n_times repeated smooth n))
  (smooth_n_times f))