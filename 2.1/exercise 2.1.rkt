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

;print
(define (print_rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))