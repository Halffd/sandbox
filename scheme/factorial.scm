#lang scheme
(define (factorial n)
  (displayln (string-append "Calling factorial(" (number->string n) ")"))
  (if (= n 0)
      (begin
        (displayln "Reached base case: factorial(0) = 1")
        1)
      (let ((result (* n (factorial (- n 1)))))
        (displayln (string-append "Computed factorial(" (number->string n) ") = " (number->string result)))
        result)))

;; Example call:
(factorial -5)
