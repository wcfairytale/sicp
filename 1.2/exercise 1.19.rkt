#lang racket

(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (is_even x) (= (remainder x 2) 0))
(define (mul a b)
  (cond ((= b 0) 0)
        ((is_even b) (mul (double a) (halve b)))
        (else (+ a (mul a (- b 1))))))
(define (square x) (mul x x))

(define (fib_iter a b p q count)
  (cond ((= count 0) b)
        ((is_even count) (fib_iter a
                                   b
                                   (+ (square p) (square q))
                                   (+ (* 2 (* p q)) (square q))
                                   (/ count 2)))
        (else (fib_iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (fib n)
  (fib_iter 1 0 0 1 n))