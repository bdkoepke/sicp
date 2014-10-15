#lang racket
(require rackunit)
(require racket/trace)

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
(define (_f a)
  (sum-of-squares (+ a 1) (* a 2)))
(test-case
 "1.1.4 Compound Procedures"
 (check-eq? (square 21) 441)
 (check-eq? (square (+ 2 5)) 49)
 (check-eq? (square (square 3)) 81)
 (check-eq? (sum-of-squares 3 4) 25)
 (check-eq? (_f 5) 136))
(test-case
 "1.1.5 The Substitution Model for Procedure Application"
 (check-eq? (_f 5) 136)
 (check-eq? (sum-of-squares (+ 5 1) (* 5 2)) 136)
 (check-eq? (+ (square 6) (square 10)) 136)
 (check-eq? (+ (* 6 6) (* 10 10)) 136)
 (check-eq? (+ 36 100) 136)
 ; Applicative order versus normal order
 (check-eq? (_f 5) 136)
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

(test-case
 "Exercise 1.2"
 (check-eqv? (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
                (* 3 (- 6 2) (- 2 7))) -37/150))

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

(define epsilon 0.0001)
(define (average x y)
  (/ (+ x y) 2))
(define (approx? x y)
  (>= (* epsilon 10) (abs (- x y))))
(define (check-approx? actual expected)
  (check-true (approx? actual expected)))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess)) (* guess epsilon)))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(test-case
 "1.1.7 Example: Square roots by Newton's Method"
 (check-approx? (sqrt 2) 1.414)
 (check-approx? (sqrt 9) 3)
 (check-approx? (sqrt (+ 100 37)) 11.705)
 (check-approx? (square (sqrt 1000)) 1000))

(define (cube x) (* x x x))
(define (cuberoot x)
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess)) (* guess epsilon)))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (cuberoot-iter guess)
    (if (good-enough? guess)
        guess
        (cuberoot-iter (improve guess))))
  (cuberoot-iter 1.0))
(test-case
 "Exercise 1.8"
 (check-approx? (cube (cuberoot 27)) 27))

(define (_factorial n)
  (if (= n 1)
      1
      (* n (_factorial (- n 1)))))
(define (factorial n)
  (define (fact-iter product counter)
    (if (> counter n)
        product
        (fact-iter (* counter product)
                   (+ counter 1))))
  (fact-iter 1 1))

(test-case
 "1.2.1 Linear Recursion and Iteration"
 (check-eq? (_factorial 6) 720)
 (check-eq? (factorial 6) 720))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
(test-case
 "Exercise 1.10"
 (check-eq? (A 1 10) 1024)
 (check-eq? (A 2 4) 65536)
 (check-eq? (A 3 3) 65536))

(define (_fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (_fib (- n 1)) (_fib (- n 2))))))
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))
(define (count-change amount)
  (define (cc amount kinds-of-coins)
    (define (first-denomination kinds-of-coins)
      (cond ((= kinds-of-coins 1) 1)
            ((= kinds-of-coins 2) 5)
            ((= kinds-of-coins 3) 10)
            ((= kinds-of-coins 4) 25)
            ((= kinds-of-coins 5) 50)))
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount
                       (- kinds-of-coins 1))
                   (cc (- amount
                          (first-denomination kinds-of-coins))
                       kinds-of-coins)))))
  (cc amount 5))
(test-case
 "1.2.2 Tree Recursion"
 (check-eq? (_fib 6) 8)
 (check-eq? (fib 6) 8)
 (check-eq? (count-change 100) 292))

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))
(define (f-iter n)
  (define (_f-iter counter a b c)
    (cond ((<= counter n)
           (_f-iter (+ counter 1)
                    (+ a
                       (* 2 b)
                       (* 3 c))
                    a b))
          (else a)))
  (cond ((< n 3) n)
        (else (_f-iter 3 2 1 0))))
(test-case
 "Exercise 1.11"
 (check-eq? (f 0) 0)
 (check-eq? (f 1) 1)
 (check-eq? (f 2) 2)
 (check-eq? (f 3) 4)
 (check-eq? (f 4) 11)
 (check-eq? (f 5) 25)
 (check-eq? (f-iter 0) 0)
 (check-eq? (f-iter 1) 1)
 (check-eq? (f-iter 2) 2)
 (check-eq? (f-iter 3) 4)
 (check-eq? (f-iter 4) 11)
 (check-eq? (f-iter 5) 25))

(define (pascal-triangle r c)
  (cond ((= c r) 1)
        ((= c 1) 1)
        (else (+ (pascal-triangle (- r 1) c)
                 (pascal-triangle (- r 1) (- c 1))))))
(test-case
 "Exercise 1.12"
 (check-eq? (pascal-triangle 1 1) 1)
 (check-eq? (pascal-triangle 2 1) 1)
 (check-eq? (pascal-triangle 2 2) 1)
 (check-eq? (pascal-triangle 3 1) 1)
 (check-eq? (pascal-triangle 3 2) 2)
 (check-eq? (pascal-triangle 3 3) 1)
 (check-eq? (pascal-triangle 4 1) 1)
 (check-eq? (pascal-triangle 4 2) 3)
 (check-eq? (pascal-triangle 4 3) 3)
 (check-eq? (pascal-triangle 4 4) 1)
 (check-eq? (pascal-triangle 5 1) 1)
 (check-eq? (pascal-triangle 5 2) 4)
 (check-eq? (pascal-triangle 5 3) 6)
 (check-eq? (pascal-triangle 5 4) 4)
 (check-eq? (pascal-triangle 5 5) 1))

(define (cc amount kinds-of-coins)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(test-case
 "Exercise 1.14"
 (check-eq? (+
             (+ 
              (+
               (+
                (+
                 (+ (cc 11 0)
                    (+ (cc 10 0)
                       (+ (cc 9 0)
                          (+ (cc 8 0)
                             (+ (cc 7 0)
                                (+ (cc 6 0)
                                   (+ (cc 5 0)
                                      (+ (cc 4 0)
                                         (+ (cc 3 0)
                                            (+ (cc 2 0)
                                               (+ (cc 1 0)
                                                  (cc 0 1)))))))))))))
                (+
                 (+ (cc 6 0)
                    (+ (cc 5 0))
                    (+ (cc 4 0))
                    (+ (cc 3 0)
                       (+ (cc 2 0)
                          (+ (cc 1 0)
                             (cc 0 1)))))
                 (+ (cc 1 1)
                    (cc -4 0))))
               (+
                (+
                 (+ (cc 1 0)
                    (cc 0 1))
                 (cc -4 0))
                (cc -4 1)))
              (cc -14 4))
             (cc -39 5)) 4))

(define (matrix m n v)
  (define _matrix (make-vector m v))
  (for ([i (in-range 0 m)])
    (vector-set! _matrix i (make-vector n v)))
  _matrix)
(define (matrix-set! matrix m n v)
  (vector-set! (vector-ref matrix m) n v))
(define (matrix-ref matrix m n)
  (vector-ref (vector-ref matrix m) n))
(define (_count-change amount)
  (define cache (matrix (+ amount 1) 5 null))
  (vector-fill! (vector-ref cache 0) 1)
  (for ([i (in-range 1 (vector-length cache))])
    (matrix-set! cache i 0 0))
  (define (cc amount kinds-of-coins)
    (define (first-denomination kinds-of-coins)
      (cond ((= kinds-of-coins 1) 1)
            ((= kinds-of-coins 2) 5)
            ((= kinds-of-coins 3) 10)
            ((= kinds-of-coins 4) 25)
            ((= kinds-of-coins 5) 50)))
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0) 
          ;((not (null? (matrix-ref cache amount (- kinds-of-coins 1))))
           ;(matrix-ref cache amount (- kinds-of-coins 1)))
          (else (+ (cc amount
                       (- kinds-of-coins 1))
                   (cc (- amount
                          (first-denomination kinds-of-coins))
                       kinds-of-coins)))))
  (cc amount 5))
(_count-change 11)