;; Define a function to check if our guess is close enough
(defn good-enough? [guess x]
  (> 0.001 (Math/abs (- guess (/ x guess)))))

;; Examples for good-enough?
(good-enough? 3.1 9)  ;; => false (not close enough)
(good-enough? 3 9)    ;; => true (close enough)

;; Helper function to calculate average of two numbers
(defn average [x y]
  (/ (+ x y) 2))

;; Examples for average
(average 2 4)   ;; => 3
(average 10 5)  ;; => 7.5

;; Function to improve our guess using Newton's method
(defn improve-guess [guess x]
  (average guess (/ x guess)))

;; Examples for improve-guess
(improve-guess 3 9)    ;; => 3 (already perfect)
(improve-guess 2.5 4)  ;; => 2.05 (closer to 2)

;; Main sqrt function using recursive iteration
(defn sqrt [x]
  (letfn [(sqrt-iter [guess]
            (if (good-enough? guess x)
              guess
              (sqrt-iter (improve-guess guess x))))]
    (sqrt-iter 1.0)))

;; Examples for sqrt
(sqrt 4)   ;; => 2.0000000929222947
(sqrt 9)   ;; => 3.00009155413138
(sqrt 16)  ;; => 4.000000636692939
(sqrt 2)   ;; => 1.4142156862745097

;; Function to round the result if it's very close to a whole number
(defn round-if-perfect [x]
  (let [rounded (Math/round (float x))]
    (if (< (Math/abs (- x rounded)) 0.0001)
      rounded
      x)))

;; Examples for round-if-perfect
(round-if-perfect 2.0000000929222947)  ;; => 2
(round-if-perfect 3.14159)             ;; => 3.14159
(round-if-perfect 5.9999)              ;; => 6

;; A more user-friendly sqrt function that rounds perfect squares
(defn nice-sqrt [x]
  (round-if-perfect (sqrt x)))

;; Examples for nice-sqrt
(nice-sqrt 4)   ;; => 2
(nice-sqrt 9)   ;; => 3
(nice-sqrt 2)   ;; => 1.4142156862745097


(assert (= 3 (average 3 3)))
