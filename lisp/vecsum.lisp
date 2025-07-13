(defun somavet (v n)
  "Recursively sum elements of vector V up to index N."
  (if (= n 0)
      0
    (+ (aref v (1- n)) (somavet v (1- n)))))
(let ((v [1 2 3 4 5 6 7 8 9 10]))
  (message "Sum of elements: %d" (somavet v (length v))))
