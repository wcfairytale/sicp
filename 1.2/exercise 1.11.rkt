#lang racket
(define (f_recur n)
  (cond ((< n 3) n)
        (else (+
               (f_recur (- n 1))
               (* 2 (f_recur (- n 2)))
               (* 3 (f_recur (- n 3)))))))

(define (f n)
  (define (f_iter a b c count)
    (if (= count 0)
        c
        (f_iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
  (f_iter 0 1 2 (- n 2)))

