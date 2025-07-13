#lang scheme
;; Factorial function
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;; Compute x^n
(define (power x n)
  (if (= n 0)
      1
      (* x (power x (- n 1)))))

;; Taylor series term for sine: (-1)^n * x^(2n+1) / (2n+1)!
(define (sine-term x n)
  (* (if (even? n) 1 -1)
     (/ (power x (+ (* 2 n) 1))
        (factorial (+ (* 2 n) 1)))))

;; Advanced sine function using Taylor series with n terms
(define (sine-taylor x terms)
  ;; Normalize angle to range [-π, π]
  (define pi 3.141592653589793)
  (define two-pi (* 2 pi))
  (define norm-x (- x (* two-pi (floor (/ (+ x pi) two-pi)))))
  
  ;; Compute Taylor series
  (define (sine-taylor-iter n result)
    (if (= n terms)
        result
        (sine-taylor-iter (+ n 1) (+ result (sine-term norm-x n)))))
  
  (sine-taylor-iter 0 0))

;; More practical version with error threshold instead of fixed terms
(define (sine-precise x epsilon)
  ;; Normalize angle to range [-π, π]
  (define pi 3.141592653589793)
  (define two-pi (* 2 pi))
  (define norm-x (- x (* two-pi (floor (/ (+ x pi) two-pi)))))
  
  ;; Compute Taylor series until term is smaller than epsilon
  (define (sine-iter n result prev-term)
    (let ((term (sine-term norm-x n)))
      (if (< (abs term) epsilon)
          (+ result term)
          (sine-iter (+ n 1) (+ result term) term))))
  
  (sine-iter 0 0 1.0))
(define pi 3.141592653589793)
;; Example usage:
(sine-taylor (/ pi 2) 10)       ; With 10 terms
(sine-precise (/ pi 2) 0.0000001) ; With error threshold
(sine-precise pi 0.0000001) ; With error threshold
(sine-precise (/ pi 4) 0.0000001) ; With error threshold
(sine-precise (* pi 2) 0.0000001) ; With error threshold
(define (cube x) (* x x x))
(define (q x) (- (* 4 (cube x)) (* 3 x)))
(define (cosine angle)
  (if (not (> (abs angle) 0.1))
      (- 1 (/ (* angle angle) 2)) ; cos(x) ≈ 1 - x²/2 for small x
      (q (cosine (/ angle 3.0)))))

(define (r x)
  (/ (- (* 3 x) (cube x))
     (- 1 (* 3 (square x)))))

(define (tang angle)
  (if (not (> (abs angle) 0.1))
      angle ; tan(x) ≈ x for small x
      (r (tang (/ angle 3.0)))))(cosine 0)
(cosine (/ pi 2))
(cosine pi)
(define (tangent angle)
  (/ (sine-precise angle 0.0000001) (cosine angle)))
(tangent 0)
(tangent pi)
(tangent (/ pi 2))
(define (square x) (* x x))
(tang 0)
(tang pi)
(tang (/ pi 2))
