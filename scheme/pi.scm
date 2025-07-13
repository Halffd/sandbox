;; Chudnovsky algorithm for calculating π to high precision
;; This uses exact rational arithmetic in Scheme

(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (- n 1)))))

(define (chudnovsky-pi-term k)
  (let ((numerator (* (factorial (* 6 k))
                      (+ 545140134 (* 13591409 k))))
        (denominator (* (factorial (* 3 k))
                        (expt (factorial k) 3)
                        (expt -640320 (* 3 k)))))
    (/ numerator denominator)))

(define (calculate-pi precision)
  (let ((k-max (ceiling (/ precision 14)))) ; Each term gives ~14 digits
    (let loop ((k 0)
               (sum 0))
      (if (> k k-max)
          (/ 426880 (sqrt 10005) sum)
          (loop (+ k 1)
                (+ sum (chudnovsky-pi-term k)))))))

;; For higher precision with standard Scheme, use this simpler algorithm
;; (Machin's formula)
(define (machin-pi precision)
  (let ((iterations (ceiling (* 2 precision))))
    (* 4 (- (* 4 (atan-series 1/5 iterations))
            (atan-series 1/239 iterations)))))

(define (atan-series x iterations)
  (let loop ((n 0)
             (term x)
             (sum 0))
    (if (>= n iterations)
        sum
        (loop (+ n 1)
              (* term (- (* x x)))
              (+ sum (/ term (+ (* 2 n) 1)))))))

;; Example usage:
;; For moderately high precision:
(define pi-approx (machin-pi 1000))

;; For display, convert to a string with the desired digits
(define (display-digits n digits)
  (let* ((str (number->string (exact->inexact n)))
         (dot-pos (string-index str #\.))
         (int-part (substring str 0 dot-pos))
         (frac-part (substring str (+ dot-pos 1))))
    (string-append int-part "." (substring frac-part 0 (min digits (string-length frac-part))))))

;; Display π to 100 digits:
(display-digits pi-approx 100)