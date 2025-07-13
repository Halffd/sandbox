#lang scheme
(define (factorial0 n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
(factorial0 6)


(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
(factorial 40)
(define (factorial2 n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
(fact-iter 4 4 5)
(define (inc a) (+ a 1))
(define (dec a) (- a 1))
(define (p a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
(p 1 555)
(+ 4 5)
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc (inc (+ (dec 3) 5)))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ (dec 2) 5))))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ (dec 1) 5)))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
(define (p2 a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
(p2 44 333333333333)
(+ 4 5)
(+ (dec 4) (inc 5))
(+ 3 6)
(+ (dec 3) (inc 6))
(+ 2 7)
(+ (dec 2) (inc 7))
(+ 1 8)
(+ (dec 1) (inc 8))
(+ 0 9)

;; The Ackermann function - a famously fast-growing function
;; used in computability theory to demonstrate non-primitive recursion
(define (A x y)
  (cond 
    ;; Base case 1: If y is 0, return 0
    ((= y 0) 0)
    
    ;; Base case 2: If x is 0, double y
    ((= x 0) (* 2 y))
    
    ;; Base case 3: If y is 1, return 2
    ;; This helps terminate the recursion when reducing y
    ((= y 1) 2)
    
    ;; Recursive case: This is where the magic/chaos happens
    ;; We compute (A x (- y 1)) first, then use that as the 2nd arg
    ;; to (A (- x 1) ...)
    (else (A (- x 1)
              (A x (- y 1))))))

;; To understand what's happening:
;; 1. (A 1 10) essentially computes 2^10 = 1024
;; 2. (A 2 4) computes 2^16 = 65536
;; 3. (A 3 3) computes 2^65536 (technically, but it simplifies to 65536)

;; The helper functions:
;; f(n) = (A 0 n) = 2n
(define (f n) (A 0 n))

;; g(n) = (A 1 n) = 2^n
(define (g n) (A 1 n))

;; h(n) = (A 2 n) = 2↑↑n (tetration - a tower of powers)
;; This grows EXTREMELY quickly
(define (h n) (A 2 n))

;; For example:
;; h(1) = 2
;; h(2) = 2^2 = 4
;; h(3) = 2^4 = 16
;; h(4) = 2^16 = 65536
;; h(5) = 2^65536 (a number with ~20,000 digits!)

(A 1 10)

(A 2 4)

(A 3 3)
(define (k n) (* 5 n n))

(f 10)
(g 10)
(h 4)
(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
(fib 18)
(fib-iter 5 5 5)
(fib-iter 5 3 8)

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
(count-change 100)
(cc 4 2)

(define (f-recursive n)
  (if (< n 3)
      n  ; Base case: if n < 3, return n
      (+ (f-recursive (- n 1))       ; f(n-1)
         (* 2 (f-recursive (- n 2))) ; 2*f(n-2)
         (* 3 (f-recursive (- n 3))) ; 3*f(n-3)
      )))
(define (f-iterative n)
  (define (iter a b c count)
    (if (= count 0)
        a  ; When count reaches 0, return a (which will be f(n))
        (iter b 
              c 
              (+ c (* 2 b) (* 3 a)) 
              (- count 1))))
  
  (if (< n 3)
      n  ; Handle base case directly
      (iter 0 1 2 (- n 2)))) ; Start with f(0)=0, f(1)=1, f(2)=2
(f-recursive 8)
(f-iterative 122)
(define (pascal row col)
  (cond ((= col 0) 1)                 ; Left edge is always 1
        ((= col row) 1)               ; Right edge is always 1
        ((and (> row 0) (> col 0))    ; Inside the triangle
         (+ (pascal (- row 1) (- col 1))    ; Value above-left
            (pascal (- row 1) col)))        ; Value above
        (else 0)))                    ; Invalid position
(pascal 4 2)
(pascal 5 2)
(pascal 6 3)
(pascal 6 6)
(pascal 8 -2)
(pascal (pasccal 10 5) (pascal 50 25))
(define (cube x) (* x x x))
(define (p0 x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))
(sine 90)
(define pi 3.141592653589793)
(sine (/ pi 2))
(sine pi)
(sine (* 2 pi))

(define (f-rec n)
        (cond
            [(< n 3) n]
            [else (+ (f-rec (- n 1)) (* (f-rec (- n 2)) 2) (* (f-rec (- n 3)) 3))]))

(define (f-iter n)
    (letrec ([iter (lambda (a b c n)
        (cond
            [(= n 0) c]
            [else (iter (+ a (* b 2) (* c 3)) a b (- n 1))]))])
    (iter 2 1 0 n)))
(f-iter 5)
(define (find_f n)
  (define (f_x x)
    (+ (x - 1) (* 2 (- n 2)) (* 3 (- n 3)))) 
  (define (f_iter n counter iter_count)
    (if (and
          (= iter_count 0)
          (< n 3)) 
      (f_x n)
      ((+ iter_count 1)
       (if (< n 3)
         (counter)
         ((+ counter (f_x n))
          (f_iter (- n 1) counter iter_count))))))
  (f_iter n 0 0))

(defun id-recursive (n)
  (cond ((= n 0) n)
    (t (1+ (id-recursive (1- n))))))

(defun id-iterative (n)
  (defun helper (x y)
    (cond ((= x 0) y)
      (t (helper (1- x) (1+ y)))))
  (id-iterative n 0))
(define (fib2 n)
  (let loop ((n n) (a 0) (b 1))
    (if (zero? n)
        a
        (loop (- n 1) b (+ a b)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 '(2 4 6 8))

(define (expt-rec b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
(expt-rec 2 4)
(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product))))
(expt 5 4)
(define (square x) (* x x))
(define (even? n)
  (= (remainder n 2) 0))


(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(fast-expt 16 25)
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(remainder 5 3)
(gcd 250 20)
(define (lcm a b)
  (/ (* (abs a) (abs b)) 
     (gcd a b)))
(define (lcm-list lst)
  (if (null? (cdr lst))
      (car lst)
      (lcm (car lst) (lcm-list (cdr lst)))))
(lcm 12 18)       ; Returns 36
(lcm-list '(2 3 4 5)) ; Returns 60
(lcm 69 5)

(define (fast-expt-iter b n)
  (iter-expt b n 1))

(define (iter-expt b n a)
  (cond ((= n 0) a)
        ((even? n) (iter-expt (square b) (/ n 2) a))
        (else (iter-expt b (- n 1) (* a b)))))
(/ (fast-expt-iter 8 59) (fast-expt-iter 10 32))
(fast-expt-iter 8 58)

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(divides? 4 4)
(smallest-divisor 37)
(find-divisor 253 88)
(define (prime? n..)
  (= n (smallest-divisor n)))
(prime? 4)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(random 5)
(define (fermat-test n)
  (define (try-it a)         
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(fermat-test 37)

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
(fast-prime? 377777777 100)
(fast-prime? 561 1)
(fast-prime? 6601 1999)
(remainder -2147483648 -1)
