#lang racket
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (loop_factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* product counter)
              (+ counter 1))))
  (iter 1 1))