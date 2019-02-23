#lang racket

(define (average x y) (/ (+ x y) 2.0))
(define (square x) (* x x))
(define (cube x) (* x x x))

;fixed-point
(define tolerance 0.00001)
(define (close_enough x y) (< (abs (- x y)) tolerance))

(define (fixed_point f first_guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close_enough guess next)
          next
          (try next))))
  (try first_guess))

;average damping
(define (average_damp f)
  (lambda (x) (average x (f x))))

;deriv
(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

;newton's method
(define (newton_transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons_method g guess)
  (fixed_point (newton_transform g) guess))

;cubic is a procedure that takes 3 params: a,b,c
;it produce a new procedure which has 1 param x and return x^3+ax^2+bx+c
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

;test: (newtons_method (cubic 1 2 3) 1.0)