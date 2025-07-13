#lang scheme



(define x (cons 1 2))
(car x)
(cdr x)
(define x (cons 1 2))

(define y (cons 3 4))

(define z (cons x y))

(car (car z))
(car (cdr z))
;((define make-rat cons)
;(define numer car)
;(define denom cdr))
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define (make-rat0 n d) (cons n d))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))


(define (numer x) (car x))

(define (denom x) (cdr x))
(define one-half (make-rat 1 2))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))


(define (make-rat n d)
  (cons n d))
(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))
(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

(print-rat (add-rat one-half (add-rat one-half one-half)))
;(define (cons x y)
;  (define (dispatch m)
;    (cond ((= m 0) x)
;          ((= m 1) y)
;          (else (error "Argument not 0 or 1 -- CONS" m))))
;  dispatch)

;(define (car z) (z 0))

;(define (cdr z) (z 1))
;(cdr (cons 1 0))
;; Basic interval representation
(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

;; Addition (unchanged)
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; Subtraction (Exercise 2.8)
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Optimized multiplication (Exercise 2.11 - Ben's 9 cases)
(define (mul-interval x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond 
      ;; Both positive
      ((and (>= xl 0) (>= yl 0))
       (make-interval (* xl yl) (* xu yu)))
      ;; Both negative  
      ((and (<= xu 0) (<= yu 0))
       (make-interval (* xu yu) (* xl yl)))
      ;; x positive, y negative
      ((and (>= xl 0) (<= yu 0))
       (make-interval (* xu yl) (* xl yu)))
      ;; x negative, y positive
      ((and (<= xu 0) (>= yl 0))
       (make-interval (* xl yu) (* xu yl)))
      ;; x spans zero, y positive
      ((and (<= xl 0) (>= xu 0) (>= yl 0))
       (make-interval (* xl yu) (* xu yu)))
      ;; x spans zero, y negative
      ((and (<= xl 0) (>= xu 0) (<= yu 0))
       (make-interval (* xu yl) (* xl yl)))
      ;; y spans zero, x positive  
      ((and (>= xl 0) (<= yl 0) (>= yu 0))
       (make-interval (* xu yl) (* xu yu)))
      ;; y spans zero, x negative
      ((and (<= xu 0) (<= yl 0) (>= yu 0))
       (make-interval (* xl yu) (* xl yl)))
      ;; Both span zero - the only case needing 4 multiplications!
      (else 
       (let ((p1 (* xl yl)) (p2 (* xl yu))
             (p3 (* xu yl)) (p4 (* xu yu)))
         (make-interval (min p1 p2 p3 p4)
                        (max p1 p2 p3 p4)))))))

;; Division with zero-span checking (Exercise 2.10)
(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) 
           (>= (upper-bound y) 0))
      (error "Division by interval spanning zero!")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
;; Basic interval representation
(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

;; Addition (unchanged)
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; Subtraction (Exercise 2.8)
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Optimized multiplication (Exercise 2.11 - Ben's 9 cases)
(define (mul-interval x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond 
      ;; Both positive
      ((and (>= xl 0) (>= yl 0))
       (make-interval (* xl yl) (* xu yu)))
      ;; Both negative  
      ((and (<= xu 0) (<= yu 0))
       (make-interval (* xu yu) (* xl yl)))
      ;; x positive, y negative
      ((and (>= xl 0) (<= yu 0))
       (make-interval (* xu yl) (* xl yu)))
      ;; x negative, y positive
      ((and (<= xu 0) (>= yl 0))
       (make-interval (* xl yu) (* xu yl)))
      ;; x spans zero, y positive
      ((and (<= xl 0) (>= xu 0) (>= yl 0))
       (make-interval (* xl yu) (* xu yu)))
      ;; x spans zero, y negative
      ((and (<= xl 0) (>= xu 0) (<= yu 0))
       (make-interval (* xu yl) (* xl yl)))
      ;; y spans zero, x positive  
      ((and (>= xl 0) (<= yl 0) (>= yu 0))
       (make-interval (* xu yl) (* xu yu)))
      ;; y spans zero, x negative
      ((and (<= xu 0) (<= yl 0) (>= yu 0))
       (make-interval (* xl yu) (* xl yl)))
      ;; Both span zero - the only case needing 4 multiplications!
      (else 
       (let ((p1 (* xl yl)) (p2 (* xl yu))
             (p3 (* xu yl)) (p4 (* xu yu)))
         (make-interval (min p1 p2 p3 p4)
                        (max p1 p2 p3 p4)))))))

;; Division with zero-span checking (Exercise 2.10)
(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) 
           (>= (upper-bound y) 0))
      (error "Division by interval spanning zero!")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
;; Let's create some intervals to play with
(define resistor1 (make-interval 6.12 7.48))   ; 6.8Ω ±10%
(define resistor2 (make-interval 4.465 4.935)) ; 4.7Ω ±5%
(define test-interval (make-interval -2 3))     ; spans zero
(define positive-int (make-interval 1 5))
(define negative-int (make-interval -10 -2))

;; Testing selectors
(lower-bound resistor1)  ; => 6.12
(upper-bound resistor1)  ; => 7.48

;; Addition example
(add-interval resistor1 resistor2)  
; => [10.585, 12.415] (total resistance in series)

;; Subtraction example  
(sub-interval resistor1 resistor2)
; => [1.185, 3.015] (difference between resistors)

;; Multiplication examples (hitting different cases!)

;; Case 1: Both positive
(mul-interval positive-int (make-interval 2 4))
; => [2, 20] (uses 2 multiplications: 1*2, 5*4)

;; Case 2: Both negative  
(mul-interval negative-int (make-interval -8 -3))
; => [6, 80] (uses 2 multiplications: -2*-3, -10*-8)

;; Case 9: Both span zero (needs all 4 multiplications!)
(mul-interval test-interval (make-interval -1 2))
; => [-4, 6] (computes all: -2*-1=2, -2*2=-4, 3*-1=-3, 3*2=6)

;; Division examples

;; Safe division
(div-interval positive-int (make-interval 2 4))
; => [0.25, 2.5] (1/4 to 5/2)

;; This would throw an error!
; (div-interval positive-int test-interval)  
; => Error: "Division by interval spanning zero!"

;; Real world example: parallel resistance calculation
;; R_parallel = (R1 * R2) / (R1 + R2)
(define parallel-resistance 
  (div-interval 
    (mul-interval resistor1 resistor2)
    (add-interval resistor1 resistor2)))

; (parallel-resistance)
; => [2.58..., 2.97...] (approximate range!)
;; Check what your intervals look like
resistor1
resistor2
positive-int

;; Test the operations
(add-interval resistor1 resistor2)
(sub-interval resistor1 resistor2) 
(mul-interval positive-int (make-interval 2 4))
(div-interval positive-int (make-interval 2 4))

;; See that parallel resistance calculation
parallel-resistance
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(center resistor1)
(width resistor1)
(make-center-width 2 2)
;; Constructor: center ± percentage
(define (make-center-percent center percent)
  (let ((tolerance (* center (/ percent 100))))
    (make-interval (- center tolerance)
                   (+ center tolerance))))

;; Selector: get the center 
(define (center interval)
  (/ (+ (lower-bound interval) (upper-bound interval)) 2))

;; Selector: get the percentage tolerance
(define (percent interval)
  (let ((c (center interval))
        (width (/ (- (upper-bound interval) (lower-bound interval)) 2)))
    (* (/ width c) 100)))

(define resistor-fancy (make-center-percent 6.8 10))  ; 6.8Ω ±10%
resistor-fancy          ; => (6.12 . 7.48)
(center resistor-fancy) ; => 6.8
(percent resistor-fancy); => 10.0
;2.13
;; Test this theory:
(define r1 (make-center-percent 100 5))    ; 100 ± 5%
(define r2 (make-center-percent 200 3))    ; 200 ± 3%
(define product (mul-interval r1 r2))

(center product)  ; ≈ 20000
(percent product) ; Should be ≈ 8% (5% + 3%)
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
(par1 resistor1 resistor2)
(par2 resistor1 resistor2)
; 3.14
;; Create a tight interval (small uncertainty)
(define A (make-center-percent 100 1))  ; 100 ± 1% = [99, 101]

;; This SHOULD be 1, right? A/A = 1 always!
(div-interval A A)  
;; => [99/101, 101/99] ≈ [0.98, 1.02] 

;; WAIT WHAT?! 🤯
(center (div-interval A A))   ; ≈ 1.0
(percent (div-interval A A))  ; ≈ 2%  (NOT 0%!)

;; Let's try A/B
(define B (make-center-percent 50 1))   ; 50 ± 1%
(div-interval A B)
;; This gives roughly [99/50.5, 101/49.5] ≈ [1.96, 2.04]
(percent (div-interval A B))  ; ≈ 2% again!

;; Method 1: par1 - the "obvious" formula
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

;; Method 2: par2 - algebraically equivalent but "better"
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; Test with tight intervals
(define r1 (make-center-percent 100 1))
(define r2 (make-center-percent 200 1))

(define result1 (par1 r1 r2))
(define result2 (par2 r1 r2))

(percent result1)  ; Bigger error!
(percent result2)  ; Tighter bounds!
;; Helper function to get interval span (width)
(define (span interval)
  (- (upper-bound interval) (lower-bound interval)))

;; WORKING Monte Carlo interval calculator! 🚀
(define (calculate-interval-simple expr int-list)
  (define first-calc #t)
  (define max-found 0)
  (define min-found 0)
  (define steps 20) ; Adjust for accuracy vs speed
  
  (define (update-bounds num)
    (if first-calc
        (begin
          (set! max-found num)
          (set! min-found num)
          (set! first-calc #f))
        (begin
          (set! max-found (max num max-found))
          (set! min-found (min num min-found)))))

  ;; Handle the A/A case (1 interval)
  (cond 
    ((= (length int-list) 1)
     (let* ((interval (car int-list))
            (start (lower-bound interval))
            (end (upper-bound interval))
            (step (/ (- end start) steps)))
       (let loop ((val start))
         (if (<= val end)
             (begin
               (update-bounds (expr val))
               (loop (+ val step)))
             'done))))
    
    ;; Handle 2-interval case (like parallel resistance)
    ((= (length int-list) 2)
     (let* ((int1 (car int-list))
            (int2 (cadr int-list))
            (start1 (lower-bound int1))
            (end1 (upper-bound int1))
            (step1 (/ (- end1 start1) steps))
            (start2 (lower-bound int2))
            (end2 (upper-bound int2))
            (step2 (/ (- end2 start2) steps)))
       (let loop1 ((val1 start1))
         (if (<= val1 end1)
             (begin
               (let loop2 ((val2 start2))
                 (if (<= val2 end2)
                     (begin
                       (update-bounds (expr val1 val2))
                       (loop2 (+ val2 step2)))
                     'done))
               (loop1 (+ val1 step1)))
             'done))))
    
    ;; Handle 3-interval case
    ((= (length int-list) 3)
     (let* ((int1 (car int-list))
            (int2 (cadr int-list))
            (int3 (caddr int-list))
            (start1 (lower-bound int1))
            (end1 (upper-bound int1))
            (step1 (/ (- end1 start1) steps))
            (start2 (lower-bound int2))
            (end2 (upper-bound int2))
            (step2 (/ (- end2 start2) steps))
            (start3 (lower-bound int3))
            (end3 (upper-bound int3))
            (step3 (/ (- end3 start3) steps)))
       (let loop1 ((val1 start1))
         (if (<= val1 end1)
             (begin
               (let loop2 ((val2 start2))
                 (if (<= val2 end2)
                     (begin
                       (let loop3 ((val3 start3))
                         (if (<= val3 end3)
                             (begin
                               (update-bounds (expr val1 val2 val3))
                               (loop3 (+ val3 step3)))
                             'done))
                       (loop2 (+ val2 step2)))
                     'done))
               (loop1 (+ val1 step1)))
             'done))))
    
    (else (error "Only 1-3 intervals supported")))
  
  (make-interval min-found max-found))

;; TEST THE BEAST! 🎯
(define A (make-center-percent 100 1))

;; The famous A/A problem
(display "=== A/A Test ===")
(newline)
(define naive-A-A (div-interval A A))
(define smart-A-A (calculate-interval-simple (lambda (a) (/ a a)) (list A)))

(display "Naive A/A: ")
(display naive-A-A)
(display " (error: ")
(display (percent naive-A-A))
(display "%)")
(newline)

(display "Smart A/A: ")
(display smart-A-A)
(display " (error: ")
(display (percent smart-A-A))
(display "%)")
(newline)

;; Parallel resistance showdown
(display "\n=== Parallel Resistance Test ===")
(newline)
(define R1 (make-center-percent 100 2))
(define R2 (make-center-percent 200 2))

(define par-naive (div-interval (mul-interval R1 R2) (add-interval R1 R2)))
(define par-smart (calculate-interval-simple 
                    (lambda (r1 r2) (/ (* r1 r2) (+ r1 r2)))
                    (list R1 R2)))

(display "Naive parallel: error ")
(display (percent par-naive))
(display "%")
(newline)
(display "Smart parallel: error ")
(display (percent par-smart))
(display "%")
(newline)

;; Complex 3-variable test
(display "\n=== Complex Expression Test ===")
(newline)
(define X (make-center-percent 10 3))
(define Y (make-center-percent 20 2))
(define Z (make-center-percent 15 4))

(define complex-result 
  (calculate-interval-simple 
    (lambda (x y z) (/ (+ (* x y) (* y z) (* x z)) (+ x y z)))
    (list X Y Z)))

(display "Complex expression result: ")
(display complex-result)
(display " (error: ")
(display (percent complex-result))
(display "%)")
(newline)
; 2..2
;; Points: basic building blocks
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

;; Line segments: built from points
(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

;; Midpoint calculation
(define (midpoint-segment seg)
  (let ((start (start-segment seg))
        (end (end-segment seg)))
    (make-point 
      (/ (+ (x-point start) (x-point end)) 2)
      (/ (+ (y-point start) (y-point end)) 2))))

;; Provided print function
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; TEST IT! 🚀
(define p1 (make-point 1 3))
(define p2 (make-point 7 9))
(define seg1 (make-segment p1 p2))

(print-point (midpoint-segment seg1))  ; Should print (4,6)
;; Helper function for distance
(define (distance p1 p2)
  (sqrt (+ (square (- (x-point p2) (x-point p1)))
           (square (- (y-point p2) (y-point p1))))))

;; REPRESENTATION 1: Opposite corners
(define (make-rectangle-corners corner1 corner2)
  (cons corner1 corner2))

(define (rect-corner1 rect) (car rect))
(define (rect-corner2 rect) (cdr rect))

;; Extract width and height for corner representation
(define (rect-width-corners rect)
  (abs (- (x-point (rect-corner2 rect)) 
          (x-point (rect-corner1 rect)))))

(define (rect-height-corners rect)
  (abs (- (y-point (rect-corner2 rect)) 
          (y-point (rect-corner1 rect)))))

;; REPRESENTATION 2: Center, width, height, and angle
(define (make-rectangle-center center width height angle)
  (cons (cons center width) (cons height angle)))

(define (rect-center rect) (car (car rect)))
(define (rect-width-center rect) (cdr (car rect)))
(define (rect-height-center rect) (car (cdr rect)))
(define (rect-angle rect) (cdr (cdr rect)))

;; ABSTRACTION LAYER: Universal selectors! 🎯
;; These work with EITHER representation!

(define (rect-width rect)
  (cond 
    ;; Check if it's the corners representation
    ((and (pair? (car rect)) 
          (number? (x-point (car rect))))
     (rect-width-corners rect))
    ;; Otherwise it's the center representation  
    (else (rect-width-center rect))))

(define (rect-height rect)
  (cond 
    ;; Check if it's the corners representation
    ((and (pair? (car rect)) 
          (number? (x-point (car rect))))
     (rect-height-corners rect))
    ;; Otherwise it's the center representation
    (else (rect-height-center rect))))

;; UNIVERSAL PROCEDURES! ✨
;; These work with ANY rectangle representation!

(define (rect-perimeter rect)
  (* 2 (+ (rect-width rect) (rect-height rect))))

(define (rect-area rect)
  (* (rect-width rect) (rect-height rect)))

;; TEST BOTH REPRESENTATIONS! 🚀

;; Corners representation
(define rect1 (make-rectangle-corners 
                (make-point 0 0) 
                (make-point 4 3)))

(display "=== Corners Rectangle ===")
(newline)
(display "Width: ") (display (rect-width rect1))
(newline)
(display "Height: ") (display (rect-height rect1))
(newline)
(display "Perimeter: ") (display (rect-perimeter rect1))
(newline)
(display "Area: ") (display (rect-area rect1))
(newline)

;; Center representation  
(define rect2 (make-rectangle-center 
                (make-point 2 1.5)  ; center
                4                   ; width
                3                   ; height  
                0))                 ; angle

(display "\n=== Center Rectangle ===")
(newline)
(display "Width: ") (display (rect-width rect2))
(newline)
(display "Height: ") (display (rect-height rect2))
(newline)
(display "Perimeter: ") (display (rect-perimeter rect2))
(newline)
(display "Area: ") (display (rect-area rect2))
(newline)

;; BONUS: More robust type checking version! 💪
(define (make-rectangle-corners-tagged corner1 corner2)
  (cons 'corners (cons corner1 corner2)))

(define (make-rectangle-center-tagged center width height angle)
  (cons 'center (cons (cons center width) (cons height angle))))

(define (rect-type rect) (car rect))
(define (rect-data rect) (cdr rect))

(define (rect-width-tagged rect)
  (cond 
    ((eq? (rect-type rect) 'corners)
     (abs (- (x-point (cdr (rect-data rect))) 
             (x-point (car (rect-data rect))))))
    ((eq? (rect-type rect) 'center)
     (cdr (car (rect-data rect))))
    (else (error "Unknown rectangle type"))))

(define (rect-height-tagged rect)
  (cond 
    ((eq? (rect-type rect) 'corners)
     (abs (- (y-point (cdr (rect-data rect))) 
             (y-point (car (rect-data rect))))))
    ((eq? (rect-type rect) 'center)
     (car (cdr (rect-data rect))))
    (else (error "Unknown rectangle type"))))
; 2.1
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (abs (gcd n d))))  ; Use absolute value of gcd
    (cond 
      ;; If denominator is negative, flip both signs
      ((< d 0) (cons (/ (- n) g) (/ (- d) g)))
      ;; Otherwise, normal case
      (else (cons (/ n g) (/ d g))))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; TEST IT! 🚀
(print-rat (make-rat 6 9))    ; 2/3
(print-rat (make-rat -6 9))   ; -2/3  
(print-rat (make-rat 6 -9))   ; -2/3 (normalized!)
(print-rat (make-rat -6 -9))  ; 2/3 (double negative = positive)

;; Encode pair (a,b) as 2^a * 3^b
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

;; Extract a by counting how many times 2 divides the number
(define (car z)
  (define (count-factor n factor)
    (if (= (remainder n factor) 0)
        (+ 1 (count-factor (/ n factor) factor))
        0))
  (count-factor z 2))

;; Extract b by counting how many times 3 divides the number  
(define (cdr z)
  (define (count-factor n factor)
    (if (= (remainder n factor) 0)
        (+ 1 (count-factor (/ n factor) factor))
        0))
  (count-factor z 3))

;; TEST THE MADNESS! 🎯
(define test-pair (cons 3 5))  ; 2^3 * 3^5 = 8 * 243 = 1944
(display "Encoded pair: ") (display test-pair)
(newline)
(display "car: ") (display (car test-pair))  ; Should be 3
(newline)  
(display "cdr: ") (display (cdr test-pair))  ; Should be 5
(newline)

;; More tests
(define p1 (cons 0 0))  ; 2^0 * 3^0 = 1
(define p2 (cons 1 0))  ; 2^1 * 3^0 = 2  
(define p3 (cons 0 1))  ; 2^0 * 3^1 = 3

(display "Pair (0,0): ") (display p1) (display " -> (") 
(display (car p1)) (display ",") (display (cdr p1)) (display ")")
(newline)
; 2.6
;; Given definitions
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; Let's derive ONE by hand! 
;; (add-1 zero) = 
;; (lambda (f) (lambda (x) (f ((zero f) x))))
;; = (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))  
;; = (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;; = (lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

;; Let's derive TWO!
;; (add-1 one) = 
;; (lambda (f) (lambda (x) (f ((one f) x))))
;; = (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
;; = (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))  
;; = (lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

;; ADDITION without using add-1!
;; The insight: Church numeral n applies f n times
;; So (+ m n) should apply f m times, then n more times!
(define (church-add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

;; TEST THE INSANITY! 🚀
;; Helper to convert Church numeral to regular number
(define (church-to-number n)
  ((n (lambda (x) (+ x 1))) 0))

(display "Zero: ") (display (church-to-number zero))
(newline)
(display "One: ") (display (church-to-number one))  
(newline)
(display "Two: ") (display (church-to-number two))
(newline)

;; Test addition
(define three (church-add one two))
(define five (church-add two three))

(display "One + Two = ") (display (church-to-number three))
(newline)
(display "Two + Three = ") (display (church-to-number five))
(newline)

;; BONUS: Multiplication is even crazier!
(define (church-mult m n)
  (lambda (f) (m (n f))))

(define six (church-mult two three))
(display "Two * Three = ") (display (church-to-number six))
(newline)

(define (cons-num a b) (* (expt 2 a) (expt 3 b)))

(define (log-base-n n x) (/ (log x) (log n)))

(define (extractor product base-to-remove kept-base)
  (if (= (remainder product base-to-remove) 0)
      (extractor (/ product base-to-remove) base-to-remove kept-base)
      (log-base-n kept-base product)))

(define (car-num product) (extractor product 3 2))
(define (cdr-num product) (extractor product 2 3))

(define (cons-num a b) (* (expt 2 a) (expt 3 b)))

;; Simpler approach - just count divisions
(define (count-factor n factor)
  (if (= (remainder n factor) 0)
      (+ 1 (count-factor (/ n factor) factor))
      0))

(define (car-num z) (count-factor z 2))
(define (cdr-num z) (count-factor z 3))
