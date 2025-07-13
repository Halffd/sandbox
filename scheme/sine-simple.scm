#lang scheme
;; Factorial function (iterative to avoid stack overflow)
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))))
  (iter 1 1))

;; Power function (iterative)
(define (power x n)
  (define (iter result counter)
    (if (= counter 0)
        result
        (iter (* result x) (- counter 1))))
  (iter 1 n))

;; Check if number is even
(define (even? n)
  (= (remainder n 2) 0))

;; Define PI constant
(define pi 3.141592653589793)

;; Normalize angle to [-π, π]
(define (normalize-angle x)
  (let* ((two-pi (* 2 pi))
         (cycles (floor (/ (+ x pi) two-pi))))
    (- x (* two-pi cycles))))

;; Taylor series sine with fixed terms
(define (sine-taylor x terms)
  (let ((norm-x (normalize-angle x)))
    (let loop ((n 0) 
               (result 0))
      (if (= n terms)
          result
          (let* ((exponent (+ (* 2 n) 1))
                 (sign (if (even? n) -1 1))
                 (term (* sign (/ (power norm-x exponent)
                                 (factorial exponent)))))
            (loop (+ n 1) (+ result term)))))))

;; Example usage (simplified version)
(define (sine-simple x)
  (sine-taylor x 8))  ;; 8 terms is usually sufficient for good precision

;; Test it
(sine-simple 0)          ;; Should be close to 0
(sine-simple (/ pi 2))   ;; Should be close to 1
(sine-simple pi)         ;; Should be close to 0
(sine-simple (* 2 pi))   ;; Should be close to 0
(sine-simple (/ pi 4))   ;; Should be close to 0.7071...