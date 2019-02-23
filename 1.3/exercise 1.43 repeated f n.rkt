#lang racket

(define (inc x) (+ x 1))
(define (square x) (* x x))

(define (even x) (= (remainder x 2) 0))

(define (compose f g)
  (lambda (x)
    (f (g x))))

;not use compose
(define (repeated f n)
  (if (= n 1)
      f
      (lambda (x)
        (f ((repeated f (- n 1)) x)))))

(define (repeated_iter f n)
  (define (iter i repeated_f)
    (if (= i 1)
        repeated_f
        (iter (- i 1)
              (lambda (x)
                (f (repeated_f x)))))
  (iter n f))

;using compose
(define (repeated2 f n)
  (if (= n 1)
      f
      (compose f
               (repeated2 f (- n 1)))))

(define (repeated_iter2 f n)
  (define (iter i repeated_f)
    (if (= i 1)
        repeated_f
        (iter (- i 1)
              (compose f repeated_f))))
  (iter n f))

; ((repeated square 2) 5)
; ((repeated_iter square 2) 5)
; ((repeated2 square 2) 5)
; ((repeated_iter2 square 2) 5)
; 625