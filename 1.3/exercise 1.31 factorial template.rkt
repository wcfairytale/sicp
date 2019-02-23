#lang racket

;square
(define (square x) (* x x))

;factorial template
(define (factorial term a next b)
  (if (> a b)
      1
      (* (term a)
         (factorial term (next a) next b))))

(define (iter_factorial term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;approximation to pi
(define (approxi_pi n)
  (define (term k) (square (/ (+ 4.0 (* 2.0 k))
                              (+ 3.0 (* 2.0 k)))))
  (define (next k) (+ k 1))
  ;(* 8 (/ (factorial term 0 next n) (+ 4 (* 2.0 n)))))
  (* 8 (/ (iter_factorial term 0 next n) (+ 4 (* 2.0 n)))))

