#lang racket

;interval
(define (make_interval x y)
  (cons x y))
(define (lower_bound interval)
  (car interval))
(define (upper_bound interval)
  (cdr interval))

;helpers
(define (span_zero interval)
  (and (<= (lower_bound interval) 0)
       (>= (upper_bound interval) 0)))

;arithmatics
(define (add_interval a b)
  (make_interval (+ (lower_bound a) (lower_bound b))
                 (+ (upper_bound a) (upper_bound b))))
(define (sub_interval a b)
  (add_interval a
                (make_interval (- upper_bound b)
                               (- lower_bound b))))

(define (mul_interval a b)
  (let ((p1 (* (lower_bound a) (lower_bound b)))
        (p2 (* (lower_bound a) (upper_bound b)))
        (p3 (* (upper_bound a) (lower_bound b)))
        (p4 (* (upper_bound a) (upper_bound b))))
    (make_interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div_interval a b)
  (if (span_zero b)
      (print "error divide by an interval that spans zero")
      (mul_interval a
                    (make_interval (/ 1.0 (upper_bound b))
                                   (/ 1.0 (lower_bound b))))))

(define (interval_width interval)
  (/ (- (upper_bound interval)
        (lower_bound interval))
     2))

;center-width
(define (make_center_width c w)
  (make_interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower_bound i) (upper_bound i)) 2 ))
(define (width i)
  (/ (- (upper_bound i) (lower_bound i)) 2))

;make-center-percent
(define (make_center_percent c p)
  (let ((w (/ (* c p) 100)))
    (make_center_width c w)))
(define (percent i)
  (/ (* (width i) 100)
     (center i)))

;test
(define (test)
  (let ((case_1 (make_center_percent 100 10))
        (case_2 (make_center_percent 50 25)))
    (cond ((not (= (percent case_1) 10)) (print "case1 wrong!"))
          ((not (= (percent case_2) 25)) (print "case_2 wrong!"))
          (else (print "success!")))))