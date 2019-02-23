#lang racket

(define (inc x) (+ x 1))
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2.0))
(define (even x) (= (remainder x 2) 0))

;power
(define (power x n)
  (define (iter i result)
    (if (= i n)
        result
        (iter (+ i 1) (* result x))))
  (iter 0 1.0))

    
;fixed-point
(define tolerance 0.00001)
(define (close_enough x y) (< (abs (- x y)) tolerance))

(define (fixed_point f first_guess)
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close_enough guess next)
          next
          (try next))))
  (try first_guess))

;average damping
(define (average_damp f)
  (lambda (x) (average x (f x))))

;repeated
(define (compose f g)
  (lambda (x)
    (f (g x))))

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

;aver_damp n times
(define (n_average_damp f n)
  (display "n=")
  (display n)
  (newline)
  ((repeated average_damp n) f))

;(floor log_n(x))
(define (floor_log n x)
  (cond ((> (/ x n) 1) (+ 1 (floor_log n (/ x n))))
        ((< (/ x n) 1) 0)
        (else 1)))

;nth_rt
(define (nth_rt n)
  (lambda (x)
    (fixed_point (n_average_damp (lambda (y)
                                   (/ x (power y (- n 1))))
                                 (floor_log 2 n))
                 1.0)))