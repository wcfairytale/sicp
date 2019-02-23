#lang racket

(define tolerance 0.00001)

(define (average x y) (/ (+ x y) 2.0))
(define (square x) (* x x))
(define (positive x) (> x 0))
(define (negative x) (< x 0))

(define (close_enough x y) (< (abs (- x y)) tolerance))

;fixed-point
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

;sqrt
(define (sqrt x)
  (fixed_point (average_damp (lambda (y) (/ x y)))
               1.0))

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

;sqrt using newton's method
(define (sqrt_nt x)
  (newtons_method (lambda (y) (- (square y) x)) 1.0))

;-----------------------------------------------

(define (fixed_point_of_transform g transform guess)
  (fixed_point (transform g) guess))

(define (sqrt2 x)
  (fixed_point_of_transform (lambda (y) (/ x y))
                            average_damp
                            1.0))

(define (sqrt_nt2 x)
  (fixed_point_of_transform (lambda (y) (- (square y) x))
                            newton_transform
                            1.0))