#lang racket

(define (runtime) (current-milliseconds))
(define (is_even x) (= (remainder x 2) 0))
(define (square x) (* x x))
(define (divides a b) (= (remainder b a) 0))

(define (next_old n) (+ n 1))
(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((is_even exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat_test n)
  (define (try_it a)
    (= (expmod a n n) a))
  (try_it (+ 1 (random(- n 1)))))

(define (is_prime_fast n times)
  (cond ((= times 0) true)
        ((fermat_test n) (is_prime_fast n (- times 1)))
        (else false)))

;(timed_prime_test 4398042316799)
;(timed_prime_test 1125899839733759)
(define (timed_prime_test n)
  (start_prime_test n (runtime)))