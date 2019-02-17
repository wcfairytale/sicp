#lang racket

;---------------------------------------
;header
;---------------------------------------
(define (is_even x) (= (remainder x 2) 0))
(define (square x) (* x x))
(define (divides a b) (= (remainder b a) 0))

;---------------------------------------
;normal prime check
;---------------------------------------
(define (find_divisor n t_divisor)
  (cond ((> (square t_divisor) n) n)
        ((divides t_divisor n) t_divisor)
        (else (find_divisor n (+ t_divisor 1)))))

(define (min_divisor n)
  (find_divisor n 2))

(define (is_prime n) (= (min_divisor n) n))

;---------------------------------------
;prime check using fermat_test
;---------------------------------------
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((is_even exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;random(n) returns [0, n-1], so 1 + random(n-1) returns [1, n-1]
(define (fermat_test n)
  (define (try_it a)
    (= (expmod a n n) a))
  (try_it (+ 1 (random(- n 1)))))

(define (is_prime_fermat n times)
  (cond ((= times 0) true)
        ((fermat_test n) (is_prime_fermat n (- times 1)))
        (else false)))

;---------------------------------------
;prime check using miller_rabin_test
;---------------------------------------
(define (is_nontrivial_square_root base m)
  (and
   (not (= base 1))
   (not (= base (- m 1)))
   (= 1 (remainder (square base) m))))

(define (expmod_mr base exp m)
  (cond ((= exp 0) 1)
        ((is_nontrivial_square_root base m) 0)
        ((is_even exp)
         (remainder (square (expmod_mr base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod_mr base (- exp 1) m))
                    m))))

;random(n) returns [0, n-1], so 1 + random(n-1) returns [1, n-1]
;carmichael numbers: 561, 1105, 1729, 2465, 2821, 6601
(define (miller_rabin_test n)
  (define (try_it a)
    (= (expmod_mr a (- n 1) n) 1))
  (try_it (+ 1 (random(- n 1)))))

(define (is_prime_mr n times)
  (cond ((= times 0) true)
        ((miller_rabin_test n) (is_prime_mr n (- times 1)))
        (else false)))
