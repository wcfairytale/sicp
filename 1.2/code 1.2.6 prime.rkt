#lang racket

(define (is_even x) (= (remainder x 2) 0))
(define (square x) (* x x))
(define (divides a b) (= (remainder b a) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((is_even exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;normal prime check
(define (is_prime n)
  (define (find_divisor t_divisor)
    (cond ((> (square t_divisor) n) n)
          ((divides t_divisor n) t_divisor)
          (else (find_divisor (+ t_divisor 1)))))
  (define (min_divisor)(find_divisor 2))
  (= (min_divisor) n))


;fast prime check which uses fermat_test
;  random(n) returns [0, n-1], so 1 + random(n-1) returns [1, n-1]
;  carmichael numbers: 561, 1105, 1729, 2465, 2821, 6601
(define (fermat_test n)
  (define (try_it a)
    (= (expmod a n n) a))
  (try_it (+ 1 (random(- n 1)))))

(define (is_prime_fast n times)
  (cond ((= times 0) true)
        ((fermat_test n) (is_prime_fast n (- times 1)))
        (else false)))
        