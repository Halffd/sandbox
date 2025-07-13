#lang scheme
(define (fibonacci n)
  (display (string-append "Computing fibonacci(" (number->string n) ")\n"))
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
(fibonacci 4)
