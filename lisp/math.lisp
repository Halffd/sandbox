(loop for (scheme-symbol fn) in
 '((number? numberp)
 (symbol? symbolp)
 (pair? consp)
 (eq? eq)
 (display-line print))
 do (setf (symbol-function scheme-symbol)
 (symbol-function fn)))

(defmacro define ((name &rest args) &body body)
 `(defun ,name ,args ,@body))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
 (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
 (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
 (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (deriv exp var)
 (cond ((number? exp) 0)
 ((variable? exp)
 (if (same-variable? exp var) 1 0))
 ((sum? exp)
 (make-sum (deriv (addend exp) var)
 (deriv (augend exp) var)))
 ((product? exp)
 (make-sum
 (make-product (multiplier exp)
 (deriv (multiplicand exp) var))
 (make-product (deriv (multiplier exp) var)
 (multiplicand exp))))
 (t
 (error "unknown expression type -- DERIV: ~a" exp))))

;; Example of NaN
(defparameter *nan* (/ 0.0 0.0)  ; This will produce NaN
(defparameter *nan2* (sqrt -1.0)) ; Square root of a negative number also produces NaN

(format t "Value of *nan*: ~a~%" *nan*)   ; Output: Value of *nan*: NaN
(format t "Value of *nan2*: ~a~%" *nan2*) ; Output: Value of *nan2*: NaN
