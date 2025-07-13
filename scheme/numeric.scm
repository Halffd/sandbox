;; Scheme's built-in numerical tower
;; Complex > Real > Rational > Integer

;; Generic arithmetic works across all numeric types
(define (demonstrate-numeric-polymorphism x y)
  (list 
    (+ x y)     ; addition works on any numbers
    (* x y)     ; multiplication too
    (/ x y)     ; division might promote types
    (= x y)))   ; comparison across types

;; Examples with different numeric types
(define int1 5)
(define int2 3)
(define rational1 3/4)
(define rational2 1/2)
(define real1 3.14159)
(define real2 2.71828)
(define complex1 3+4i)
(define complex2 1-2i)

;; All these work with the same generic functions
(display "Integer arithmetic: ")
(display (demonstrate-numeric-polymorphism int1 int2))
(newline)

(display "Rational arithmetic: ")
(display (demonstrate-numeric-polymorphism rational1 rational2))
(newline)

(display "Real arithmetic: ")
(display (demonstrate-numeric-polymorphism real1 real2))
(newline)

(display "Complex arithmetic: ")
(display (demonstrate-numeric-polymorphism complex1 complex2))
(newline)

;; Mixed-type arithmetic (automatic type promotion)
(display "Mixed integer + rational: ")
(display (+ int1 rational1))  ; Result: 23/4
(newline)

(display "Mixed rational * real: ")
(display (* rational1 real1))  ; Result: 2.3561925 (approximate)
(newline)

(display "Mixed real + complex: ")
(display (+ real1 complex1))  ; Result: 6.14159+4i
(newline)