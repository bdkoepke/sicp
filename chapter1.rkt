#lang racket
(require rackunit)
(test-case
 "1.1.1 Expressions"
 (check-eq? (+ 137 349) 486)
 (check-eq? (- 1000 334) 666)
 (check-eq? (* 5 99) 495)
 (check-eq? (/ 10 5) 2)
 (check-eqv? (+ 2.7 10) 12.7)
 (check-eq? (+ 21 35 12 7) 75)
 (check-eq? (* 25 4 12) 1200)
 (check-eq? (+ (* 3 5) (- 10 6)) 19)
 (check-eq? (+ (* 3
                  (+ (* 2 4)
                     (+ 3 5)))
               (+ (- 10 7)
                  6)) 57))
(define size 2)
(define pi 3.14159)
(define radius 10)
(define circumference (* 2 pi radius))
(test-case
 "1.1.2 Naming and the Environment"
 (check-eq? size 2)
 (check-eq? (* 5 size) 10)
 (check-eqv? (* pi (* radius radius)) 314.159)
 (check-eqv? circumference 62.8318))

(test-case
 "1.1.3 Evaluating Combinations"
 (check-eq? (* (+ 2 (* 4 6))
               (+ 3 5 7)) 390))

(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(test-case
 "1.1.4 Compound Procedures"
 (check-eq? (square 21) 441)
 (check-eq? (square (+ 2 5)) 49)
 (check-eq? (square (square 3)) 81)
 (check-eq? (sum-of-squares 3 4) 25)
 (check-eq? (f 5) 136))
(test-case
 "1.1.5 The Substitution Model for Procedure Application"
 (check-eq? (f 5) 136)
 (check-eq? (sum-of-squares (+ 5 1) (* 5 2)) 136)
 (check-eq? (+ (square 6) (square 10)) 136)
 (check-eq? (+ (* 6 6) (* 10 10)) 136)
 (check-eq? (+ 36 100) 136)
 ; Applicative order versus normal order
 (check-eq? (f 5) 136)
 (check-eq? (sum-of-squares (+ 5 1) (* 5 2)) 136)
 (check-eq? (+ (square (+ 5 1)) (square (* 5 2))) 136)
 (check-eq? (+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2))) 136)
 (check-eq? (+ (* 6 6) (* 10 10)) 136)
 (check-eq? (+ 36 100) 136))

(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (lt-10-gt-5? x)
  (and (> x 5) (< x 10)))
(define (>= x y)
  (not (< x y)))
(test-case
 "1.1.6 Conditional Expressions and Predicates"
 (check-true (lt-10-gt-5? 6)))

(define a 3)
(define b (+ a 1))
(test-case
 "Exercise 1.1"
 (check-eq? 10 10)
 (check-eq? (+ 5 3 4) 12)
 (check-eq? (- 9 1) 8)
 (check-eq? (/ 6 2) 3)
 (check-eq? (+ (* 2 4) (- 4 6)) 6)
 (check-eq? (+ a b (* a b)) 19)
 (check-false (= a b))
 (check-eq? (if (and (> b a) (< b (* a b)))
                b
                a) b)
 (check-eq? (cond ((= a 4) 6)
                  ((= b 4) (+ 6 7 a))
                  (else 25)) 16)
 (check-eq? (+ 2 (if (> b a) b a)) 6)
 (check-eq? (* (cond ((> a b) a)
                     ((< a b) b)
                     (else -1))
               (+ a 1)) 16))

(define (exercise-1-3 x y z)
  (if (> x y)
      (if (> y z)
          (sum-of-squares x y)
          (sum-of-squares x z))
      (if (> x z)
          (sum-of-squares y x)
          (sum-of-squares y z))))
(test-case
 "Exercise 1.3"
 (check-eq? (exercise-1-3 1 2 3) 13)
 (check-eq? (exercise-1-3 2 2 3) 13)
 (check-eq? (exercise-1-3 2 3 3) 18)
 (check-eq? (exercise-1-3 3 2 1) 13)
 (check-eq? (exercise-1-3 2 3 1) 13)
 (check-eq? (exercise-1-3 2 1 3) 13)
 (check-eq? (exercise-1-3 3 1 2) 13))

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(test-case
 "Exercise 1.4"
 (check-eq? (a-plus-abs-b 5 -10) 15))

(define epsilon 0.001)
(define (sqrt-iter x guess)
  (if (good-enough? x guess)
      guess
      (sqrt-iter x (improve x guess))))
(define (improve x guess)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? x guess)
  (< (abs (- (square guess) x)) epsilon))
(define (sqrt x)
  (sqrt-iter x 1.0))
(define (approx? x y)
  (>= epsilon (abs (- x y))))
(define (check-approx? actual expected)
  (check-true (approx? actual expected)))
(test-case
 "1.1.7 Example: Square roots by Newton's Method"
 (check-approx? (sqrt 2) 1.414)
 (check-approx? (sqrt 9) 3)
 (check-approx? (sqrt (+ 100 37)) 11.705)
 (check-approx? (square (sqrt 1000)) 1000))
