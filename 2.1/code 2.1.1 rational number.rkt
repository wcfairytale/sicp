#lang racket
(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (sign x)
  (/ x (abs x)))

;rational
(define (make_rat n d)
  (let ((g (gcd (abs n) (abs d)))
        (_sign (sign (* n d))))
    (cons (* _sign (abs (/ n g)))
          (abs (/ d g)))))
(define (numer x) (car x))
(define (denom x) (cdr x))

;rational arithmetics
(define (add_rat x y)
  (make_rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub_rat x y)
  (make_rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul_rat x y)
  (make_rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div_rat x y)
  (make_rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal_rat x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;print
(define (print_rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))