
(define size 2)
(* 4 size)
(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(Sum-of-squares 3 4)
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(define (abss x)
  (cond ((> x 7) x)
        ((= x 1) 0)
        ((< x 6) (- x))))

(abss 7)
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))
(abs -44)
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define x (square (abs -72)))
(and (> x 5) (< x 10))
(and (> x 5) (< x 1909999900))

(define x 4)
(define y 4)
(define (>= x y)
(or (> x y) (= x y)))
(define x 7)
(define (>= x y)
  (not (< x y)))


(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(display 'if)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; (define (p) (p))
(define (p) 42)  ; or some other base case value
(display 'test)
(define (test x y)
  (if (= x 0)
      0
      y))
(test 0 (p))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 9)
(sqrt 4)
(sqrt 1024)
(sqrt 2)
(sqrt 0)
(sqrt 1)
(sqrt 27)
; (sqrt -1)
(square (sqrt 1000))
(sqrt 26902776176295786352417650625)
(sqrt 0.0000000004)
(define (square x) (* x x))

(define (good-enough? previous-guess guess)
  (< (abs (/ (- guess previous-guess) guess)) 0.00000000001))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))x

(define (sqrt x)
  (sqrt-iter 1.0 x))
(display "Exercise 1.7 ")
(sqrt 123456789012345) ; = 11111111.061111081 - Error: 0.015625
(sqrt 0.00000000123456)
(good-enough? 4 2)
; (sqrt 123456789012345)
(sqrt 4)
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
(define (sqrt x)

 (define (good-enough? guess last-guess)
 (< (abs (- guess last-guess)) 0.0000000000000000001))

 (define (improve guess)
 (average guess (/ x guess)))

 (define (sqrt-iter guess last-guess)
 (display "guess: ")
 (display guess) (newline)
 (if (good-enough? guess last-guess)
 guess
 (sqrt-iter (improve guess) guess)))

 (sqrt-iter (/ x 2.0) 0))

(define (average x y)
 (/ (+ x y) 2.0))
(sqrt 9)
(sqrt (+ (sqrt 2) (sqrt 3)))
(sqrt 26902776176295786352417650625)
(sqrt 0.0000000004)
(sqrt 123456789012345)
(square (sqrt 0.00000000123))

(define (square n) (* n n))

(define (average x y) (/ (+ x y) 2))

; old good-enough? procedure
; (define (good-enough? g n)
;  (< (abs (- (square g) n)) 0.001))

; new good-enough? procedure
(define (good-enough? g g.)
  (> (/ (min g g.) (max g g.)) 0.999999999))

(define (improve g n) (average g (/ n g)))

(define (square-root n)
  (define (try n g g.)
    (if (good-enough? g g.)
        g
        (try n (improve g n) g)))
  (try n 1.0 0.0))
(square-root 0.0005)
(define (cube-root x)
  (define (cube guess)
    (* guess guess guess))

  (define (square guess)
    (* guess guess))

  (define (abs value)
    (if (< value 0) (- value) value))

  (define (good-enough? guess target)
    (< (abs (- (cube guess) target)) 0.001)) ; Adjusted to check the difference

  (define (improve-guess guess target)
    (/ (+ (/ target (square guess)) (* 2 guess)) 3))

  (define (cube-iter guess target)
    (if (good-enough? guess target)
        guess
        (cube-iter (improve-guess guess target) target)))

  (cube-iter 1.0 x)) ; Start with an initial guess of 1.0

(cube-root 27) ; Example usage
(cube-root 2)
(cube-root 125)
; Find the square root of a number iteratively
(define (sqrt x)
 (sqrt-iter starting-guess x))

; What do you want the minimum different to be
(define goal 0.01)

; Give a starting value to guess from
(define starting-guess 1)

; The actual guess function
(define (sqrt-iter guess x)
 (if (good-enough? guess x)
 guess
 (sqrt-iter (improve guess x)
 x)))

; Set a goal
(define (good-enough? guess x)
 (< (abs (- (square guess) x)) goal))

; Improve the guess iteratively
(define (improve guess x)
 (average guess (/ x guess)))

; Just get the average
(define (average x y)
 (/ (+ x y) 2))

; Cube root of a number iteratively
(define (cube-root x)
 (cube-root-iter 1.0 x))

; The actual iterative computation of the cube root
(define (cube-root-iter guess x)
 (if (good-enough-cube? guess x)
 guess
 (cube-root-iter (improve guess x)
 x)))

;(cube-root 3)
(define (cube x) (* x x x))

(define (good-enough? previous-guess guess)
  (< (abs (/ (- guess previous-guess) guess)) 0.00000000001))

(define (cube-root-iter guess x)
  (if (good-enough? (improve guess x) guess)
      guess
      (cube-root-iter (improve guess x) x)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))


(define (cube-root x)
  (cube-root-iter 1.0 x))

; Basic testing
(define x 12345)
(define cube-root-x (cube-root x))
(newline)
(display "(cube-root ") (display x) (display ") -> ") (display cube-root-x)
(newline)
(display "(cube ") (display cube-root-x) (display ") -> ") (display (cube cube-root-x))
(define (square x) (* x x))

(define (square x) 
  (display (log x))
  (exp (double (log x))))

(define (double x) (+ x x))
(square 2)
(square 9)
(square 0)
(square -2)
(define (sqrt x)
  (define (square n) (* n n))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(sqrt 81)
(sqrt 100)
