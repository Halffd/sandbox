;; Custom numerical tower implementation
(define-record-type number-base
  (make-number-base type value)
  number-base?
  (type number-type)
  (value number-value))

;; Constructors for different number types
(define (make-integer n)
  (make-number-base 'integer n))

(define (make-rational num den)
  (make-number-base 'rational (cons num den)))

(define (make-real r)
  (make-number-base 'real r))

(define (make-complex real-part imag-part)
  (make-number-base 'complex (cons real-part imag-part)))

;; Generic addition that works across all our number types
(define (generic-add x y)
  (let ((x-type (number-type x))
        (y-type (number-type y))
        (x-val (number-value x))
        (y-val (number-value y)))
    
    (cond
      ;; Both integers
      ((and (eq? x-type 'integer) (eq? y-type 'integer))
       (make-integer (+ x-val y-val)))
      
      ;; Both rationals
      ((and (eq? x-type 'rational) (eq? y-type 'rational))
       (let ((x-num (car x-val)) (x-den (cdr x-val))
             (y-num (car y-val)) (y-den (cdr y-val)))
         (make-rational (+ (* x-num y-den) (* y-num x-den))
                       (* x-den y-den))))
      
      ;; Mixed integer and rational
      ((and (eq? x-type 'integer) (eq? y-type 'rational))
       (generic-add (make-rational x-val 1) y))
      
      ((and (eq? x-type 'rational) (eq? y-type 'integer))
       (generic-add x (make-rational y-val 1)))
      
      ;; Add more cases for real and complex...
      (else (error "Unsupported number type combination")))))

;; Usage showing polymorphic behavior
(define n1 (make-integer 5))
(define n2 (make-integer 3))
(define r1 (make-rational 3 4))
(define r2 (make-rational 1 2))

;; Same function works on different types
(display "Integer + Integer: ")
(display (number-value (generic-add n1 n2)))  ; 8
(newline)

(display "Rational + Rational: ")
(let ((result (generic-add r1 r2)))
  (display (car (number-value result)))
  (display "/")
  (display (cdr (number-value result))))  ; 5/4
(newline)

(display "Integer + Rational: ")
(let ((result (generic-add n1 r1)))
  (display (car (number-value result)))
  (display "/")
  (display (cdr (number-value result))))  ; 23/4
(newline)