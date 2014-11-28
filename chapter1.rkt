#lang racket
(require rackunit)
(require racket/trace)

(define (++ x) (+ x 1))
(define (-- x) (- x 1))

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

(define pi 3.14159)
(define radius 10)
(define circumference (* 2 pi radius))
(test-case
 "1.1.2 Naming and the Environment"
 (define size 2)
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
(test-case
 "1.1.4 Compound Procedures"
 (define (f a)
   (sum-of-squares (++ a) (* a 2)))
 (check-eq? (square 21) 441)
 (check-eq? (square (+ 2 5)) 49)
 (check-eq? (square (square 3)) 81)
 (check-eq? (sum-of-squares 3 4) 25)
 (check-eq? (f 5) 136))

(test-case
 "1.1.5 The Substitution Model for Procedure Application"
 (define (f a)
   (sum-of-squares (++ a) (* a 2)))
 (check-eq? (f 5) 136)
 (check-eq? (sum-of-squares (++ 5) (* 5 2)) 136)
 (check-eq? (+ (square 6) (square 10)) 136)
 (check-eq? (+ (* 6 6) (* 10 10)) 136)
 (check-eq? (+ 36 100) 136)
 ; Applicative order versus normal order
 (check-eq? (f 5) 136)
 (check-eq? (sum-of-squares (++ 5) (* 5 2)) 136)
 (check-eq? (+ (square (++ 5)) (square (* 5 2))) 136)
 (check-eq? (+ (* (++ 5) (++ 5)) (* (* 5 2) (* 5 2))) 136)
 (check-eq? (+ (* 6 6) (* 10 10)) 136)
 (check-eq? (+ 36 100) 136))

(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (>= x y)
  (not (< x y)))
(test-case
 "1.1.6 Conditional Expressions and Predicates"
 (define (lt-10-gt-5? x)
   (and (> x 5) (< x 10)))
 (check-true (lt-10-gt-5? 6)))

(test-case
 "Exercise 1.1"
 (define a 3)
 (define b (++ a))
 (check-eq? 10 10)
 (check-eq? (+ 5 3 4) 12)
 (check-eq? (-- 9) 8)
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
               (++ a)) 16))

(test-case
 "Exercise 1.2"
 (check-eqv? (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
                (* 3 (- 6 2) (- 2 7))) -37/150))

(test-case
 "Exercise 1.3"
 (define (exercise-1-3 x y z)
   (if (> x y)
       (if (> y z)
           (sum-of-squares x y)
           (sum-of-squares x z))
       (if (> x z)
           (sum-of-squares y x)
           (sum-of-squares y z))))
 (check-eq? (exercise-1-3 1 2 3) 13)
 (check-eq? (exercise-1-3 2 2 3) 13)
 (check-eq? (exercise-1-3 2 3 3) 18)
 (check-eq? (exercise-1-3 3 2 1) 13)
 (check-eq? (exercise-1-3 2 3 1) 13)
 (check-eq? (exercise-1-3 2 1 3) 13)
 (check-eq? (exercise-1-3 3 1 2) 13))

(test-case
 "Exercise 1.4"
 (define (a-plus-abs-b a b)
   ((if (> b 0) + -) a b))
 (check-eq? (a-plus-abs-b 5 -10) 15))

(define epsilon 0.0001)
(define (average x y)
  (/ (+ x y) 2))
(define (check-approx? actual expected)
  (define (approx? x y)
    (>= (* epsilon 10) (abs (- x y))))
  (check-true (approx? actual expected)))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess)) (* guess epsilon)))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (_sqrt guess)
    (if (good-enough? guess)
        guess
        (_sqrt (improve guess))))
  (_sqrt 1.0))
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

(test-case
 "1.2.1 Linear Recursion"
 (define (factorial n)
   (if (= n 1)
       1
       (* n (factorial (- n 1)))))
 (check-eq? (factorial 6) 720))
(test-case
 "1.2.1 Iterative"
 (define (factorial n)
   (define (fact-iter product counter)
     (if (> counter n)
         product
         (fact-iter (* counter product)
                    (+ counter 1))))
   (fact-iter 1 1))
 (check-eq? (factorial 6) 720))

(test-case
 "Exercise 1.10"
 (define (A x y)
   (cond ((= y 0) 0)
         ((= x 0) (* 2 y))
         ((= y 1) 2)
         (else (A (- x 1) (A x (- y 1))))))
 (check-eq? (A 1 10) 1024)
 (check-eq? (A 2 4) 65536)
 (check-eq? (A 3 3) 65536))

(test-case
 "1.2.2 Tree Recusion, Recursive"
 (define (fib n)
   (cond ((= n 0) 0)
         ((= n 1) 1)
         (else (+ (fib (-- n)) (fib (- n 2))))))
 (check-eq? (fib 6) 8))
(test-case
 "1.2.2 Tree Recursion, Iterative"
 (define (fib n)
   (define (fib-iter a b count)
     (if (= count 0)
         b
         (fib-iter (+ a b) a (-- count))))
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
                        (-- kinds-of-coins))
                    (cc (- amount
                           (first-denomination kinds-of-coins))
                        kinds-of-coins)))))
   (cc amount 5))
 (check-eq? (fib 6) 8)
 (check-eq? (count-change 100) 292))


(test-case
 "Exercise 1.11 Recursive"
 (define (f n)
   (cond ((< n 3) n)
         (else (+ (f (-- n))
                  (* 2 (f (- n 2)))
                  (* 3 (f (- n 3)))))))
 (check-eq? (f 0) 0)
 (check-eq? (f 1) 1)
 (check-eq? (f 2) 2)
 (check-eq? (f 3) 4)
 (check-eq? (f 4) 11)
 (check-eq? (f 5) 25))
(test-case
 "Exercise 1.11 Iterative"
 (define (f n)
   (define (_f counter a b c)
     (cond ((<= counter n)
            (_f (++ counter)
                (+ a
                   (* 2 b)
                   (* 3 c))
                a b))
           (else a)))
   (cond ((< n 3) n)
         (else (_f 3 2 1 0))))
 (check-eq? (f 0) 0)
 (check-eq? (f 1) 1)
 (check-eq? (f 2) 2)
 (check-eq? (f 3) 4)
 (check-eq? (f 4) 11)
 (check-eq? (f 5) 25))

(test-case
 "Exercise 1.12"
 (define (pascal-triangle r c)
   (cond ((= c r) 1)
         ((= c 1) 1)
         (else (+ (pascal-triangle (-- r) c)
                  (pascal-triangle (-- r) (-- c))))))
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

(test-case
 "Exercise 1.14"
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
                      (-- kinds-of-coins))
                  (cc (- amount
                         (first-denomination kinds-of-coins))
                      kinds-of-coins)))))
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

(define (make-matrix m n v)
  (define _make-matrix (make-vector m v))
  (for ([i (in-range 0 m)])
    (vector-set! _make-matrix i (make-vector n v)))
  _make-matrix)
(define (matrix-set! matrix m n v)
  (vector-set! (vector-ref matrix m) n v))
(define (matrix-ref matrix m n)
  (vector-ref (vector-ref matrix m) n))

(define (count-change amount)
  (define table (make-matrix (++ amount) 5 null))
  (vector-fill! (vector-ref table 0) 1)
  (for ([i (in-range 1 (vector-length table))])
    (matrix-set! table i 0 0))
  
  (define (cc amount kinds-of-coins)
    (define (first-denomination kinds-of-coins)
      (cond ((= kinds-of-coins 1) 1)
            ((= kinds-of-coins 2) 5)
            ((= kinds-of-coins 3) 10)
            ((= kinds-of-coins 4) 25)
            ((= kinds-of-coins 5) 50)))
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0) 
          ;((not (null? (matrix-ref table amount (- kinds-of-coins 1))))
          ; (matrix-ref table amount (- kinds-of-coins 1)))
          (else (+ (cc amount
                       (-- kinds-of-coins))
                   (cc (- amount
                          (first-denomination kinds-of-coins))
                       kinds-of-coins)))))
  (cc amount 5))

(define (sine angle)
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))
(test-case
 "Exercise 1.15"
 (check-approx? (sine 12.5) -0.060813768577286265))

(define (__expt b n)
  (if (= n 0)
      1
      (* b (__expt b (- n 1)))))
(define (_expt b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b
                   (- counter 1)
                   (* b product))))
  (expt-iter b n 1))
(define (even? n)
  (= (remainder n 2) 0))
(define (expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt b (/ n 2))))
        (else (* b (expt b (- n 1))))))
(define (fast-expt b n)
  (define (expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (expt-iter (square b) (/ n 2) a))
          (else (expt-iter b (- n 1) (* a b)))))
  (expt-iter b n 1))
(test-case
 "Exercise 1.16"
 (check-eq? (fast-expt 2 2) 4)
 (check-eq? (fast-expt 2 3) 8)
 (check-eq? (fast-expt 2 10) 1024)
 (check-eq? (fast-expt 3 3) 27)
 (check-eq? (fast-expt 5 5) 3125))

(test-case
 "Exercise 1.18"
 (define (* multiplicand multiplier)
   (define (double x)
     (+ x x))
   (define (halve x)
     (arithmetic-shift x -1))
   (define (*-iter multiplicand multiplier product)
     (cond ((= multiplier 0) product)
           ((even? multiplier) (*-iter (double multiplicand) (halve multiplier) product))
           (else (*-iter multiplicand (- multiplier 1) (+ product multiplicand)))))
   (*-iter multiplicand multiplier 0))
 (check-eq? (* 6 7) 42)
 (check-eq? (* 7 6) 42)
 (check-eq? (* 12 12) 144)
 (check-eq? (* 5 12) 60)
 (check-eq? (* 12 5) 60))

(test-case
 "Exercise 1.19")

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(test-case
 "1.2.5 Greatest Common Divisors"
 (check-eq? (gcd 206 40) 2))
(test-case
 "Exercise 1.20"
 ; Normal Order Evaluation
 (check-eq? (gcd 206 40)
            2)
 (check-eq? (if (= 40 0)
                206
                (gcd 40 (remainder 206 40)))
            2)
 (check-eq? (if (= (remainder 206 40) 0)
                40
                (gcd (remainder 206 40)
                     (remainder 40 (remainder 206 40))))
            2)
 (check-eq? (if (= (remainder 40 (remainder 206 40)) 0)
                (remainder 206 40)
                (gcd (remainder 40 (remainder 206 40))
                     (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
            2)
 (check-eq? (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
                (remainder 40 (remainder 206 40))
                (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
                     (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
            2)
 
 (check-eq? (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)
                (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
                (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
                     (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
            2)
 ; Applicative Order Evaluation
 (check-eq? (gcd 206 40)
            2)
 (check-eq? (gcd 40 (remainder 206 40))
            2)
 (check-eq? (gcd 6 (remainder 40 6))
            2)
 (check-eq? (gcd 4 (remainder 6 4))
            2)
 (check-eq? (gcd 2 (remainder 4 2))
            2)
 (check-eq? (gcd 2 (remainder 2 2))
            2))

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (define (divides? a b)
      (= (remainder b a) 0))
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))
(define (prime? n)
  (= n (smallest-divisor n)))
(test-case
 "1.2.6 Example: Testing for Primality"
 (check-eq? (prime? 7) true)
 (check-eq? (prime? 23) true)
 (check-eq? (prime? 21) false)
 (check-eq? (prime? 77) false))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
(test-case
 "Exercise 1.21"
 (check-eq? (smallest-divisor 199) 199)
 (check-eq? (smallest-divisor 1999) 1999)
 (check-eq? (smallest-divisor 19999) 7))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))
(define (start-prime-test n start-time)
  (cond ((prime? n)
      (report-prime (- (current-milliseconds) start-time)))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (cube x) (* x x x))

(define (sum-integers a b)
	(if (> a b)
		0
		(+ a (sum-integers (+ a 1) b))))

(define (sub-cubes a b)
	(if (> a b)
		0
		(+ (cube a)
		   (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
	(if (> a b)
		0
		(+ (/ 1.0 (* a (+ a 2)))
			(pi-sum (+ a 4) b))))

(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a)
			(sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
	(sum cube a inc b))
(sum-cubes 1 10)

(define (identity x) x)
(define (sum-integers a b)
	(sum identity a inc b))
(sum-integers 1 10)
(define (pi-sum a b)
	(define (pi-term x)
		(/ 1.0 (* x (+ x 2))))
	(define (pi-next x)
		(+ x 4))
	(sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))

(define (integral f a b dx)
	(define (add-dx x)
		(+ x dx))
	(* (sum f (+ a (/ dx 2.0)) add-dx b)
		dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
