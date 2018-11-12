#lang racket

;header
(define (is_even x) (= (remainder x 2) 0))
(define (square x) (* x x))
(define (divides a b) (= (remainder b a) 0))
(define (runtime) (current-inexact-milliseconds))

;fast prime check which uses fermat_test
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

(define (report_prime n result elapsed_time)
  (display n)
  (if result (display " is prime. ") (display " is not prime. "))
  (display "time_used:") (display elapsed_time)
  (newline))

(define (start_prime_test n start_time)
  (if (is_prime_fast n 2)
      (report_prime n #t (- (runtime) start_time))
      (report_prime n #f (- (runtime) start_time))))

(define (timed_prime_test n)
  (start_prime_test n (runtime)))
        
;---------------------------------------
;exercise 1.24
;---------------------------------------
(define (get_prime i)
  (cond ((= i 1) 1009)
        ((= i 2) 1013)
        ((= i 3) 1019)
        ((= i 4) 10007)
        ((= i 5) 10009)
        ((= i 6) 10037)
        ((= i 7) 100003)
        ((= i 8) 100019)
        ((= i 9) 100043)
        ((= i 10) 1000003)
        ((= i 11) 1000033)
        ((= i 12) 1000037)))

(define (get_prime2 i)
  (cond ((= i 1) 100000000003)
        ((= i 2) 100000000019)
        ((= i 3) 100000000057)
        ((= i 4) 1000000000039)
        ((= i 5) 1000000000061)
        ((= i 6) 1000000000063)
        ((= i 7) 10000000000037)
        ((= i 8) 10000000000051)
        ((= i 9) 10000000000099)
        ((= i 10) 100000000000031)
        ((= i 11) 100000000000067)
        ((= i 12) 100000000000097)))
         
(define (test)
  (define (test_iter i)
    (cond ((> i 12)(display "test over"))
          (else (timed_prime_test (get_prime i))
                (test_iter (+ i 1)))))
  (test_iter 1))
        