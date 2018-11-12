#lang racket

(define (is_even x) (= (remainder x 2) 0))
(define (square x) (* x x))
(define (divides a b) (= (remainder b a) 0))
(define (runtime) (current-inexact-milliseconds))
(define (next_odd x)
  (if (is_even x) (+ x 1) (+ x 2)))

(define (report_prime n result elapsed_time)
  (display n)
  (if result (display " is prime. ") (display " is not prime. "))
  (display "time_used:") (display elapsed_time)
  (newline))

;normal prime check
(define (find_divisor n t_divisor)
  (cond ((> (square t_divisor) n) n)
        ((divides t_divisor n) t_divisor)
        (else (find_divisor n (+ t_divisor 1)))))

(define (min_divisor n)
  (find_divisor n 2))

(define (is_prime n) (= (min_divisor n) n))

(define (start_prime_test n start_time)
  (if (is_prime n)
      (report_prime n #t (- (runtime) start_time))
      (report_prime n #f (- (runtime) start_time))))

;4398042316799
;1125899839733759
(define (timed_prime_test n)
  (start_prime_test n (runtime)))

;(search_for_primes 100000000000 1000000000000 3)
;(search_for_primes 1000000000000 10000000000000 3)
;(search_for_primes 10000000000000 100000000000000 3)
;(search_for_primes 100000000000000 1000000000000000 3)
(define (search_for_primes begin end max_count)
  (define (search_iter n begin_time count)
    (cond ((or (> n end)(= count max_count))(display "are primes."))
          ((is_prime n)
           (report_prime n #t (- (runtime) begin_time))
           (search_iter (next_odd n) (runtime) (+ count 1)))
          (else (search_iter (next_odd n) (runtime) count))))
  (search_iter begin (runtime) 0))