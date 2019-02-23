#lang racket

;cube
(define (cube x) (* x x x))

;even
(define (even x) (= (remainder x 2) 0))

;identity
(define (identity x) x)

;sum template
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;Integration using Simpson's Rule
(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (fact k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even k) 2)
          (else 4)))
  (define (term k) (* (fact k) (f (+ a (* k h)))))
  (define (next k) (+ k 1))
  (* (/ h 3) (sum term 0 next n)))

