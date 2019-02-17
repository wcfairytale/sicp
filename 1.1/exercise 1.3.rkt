#lang racket
(define (max a b) (if (> a b) a b))
(define (min a b) (if (< a b) a b))
(define (square a) (* a a ))

;all - min
(define (sum_bigger_2_square a b c)
  (- (+ (square a)
        (square b)
        (square c))
     (square (min (min a b) c))))

;strategy tree
(define (sum_bigger_2_square2 a b c)
  (if (< a b)
      (if (< a c)
          (+ (square b) (square c))
          (+ (square b) (square a)))
      (if (< b c)
          (+ (square a) (square c))
          (+ (square a) (square b)))))

;bigger 2
(define (sum_bigger_2_square3 a b c)
  (+ (square (max a b))
     (square (max (min a b) c))))
