;; Example usage of Common Lisp format directives
(format t "~:@(int case example~)~%")              ; Capitalize each word
(format t "Number with commas ~:D~%" 100000000)    ; Insert commas
(format t "PI to 5 characters ~5F~%" 3.141593)    ; Width of 5 characters
(format t "PI to 4 decimals ~,4F~%" 3.141593)     ; 4 decimal places
(format t "10 Percent ~,,2F~%" 0.10)              ; Scale factor 100 (10^2)
(format t "10 Dollars ~$~%" 10)                    ; Monetary format
(format t "7/8 = ~d~%"(mod 7 8))
(format t "8/5 = ~d~%"(mod 8 5))
(format t "8/5 = ~d~%"(rem 8 5))
(format t "11 / -222 = ~d~%" (rem 11 -222))
(setq *print-case* :capitalize)

(format t "expt 4 2 = ~d~%" (expt 4 2))
(format t "sqrt 81 = ~d~%" (sqrt 81))
(format t "exp 1 = ~d~%" (exp 1))
(format t "log 1000 10 = ~d~%" (log 1000 10)) ; = 3 = base 10
(format t "eq 'dog 'dog = ~d~%" (eq 'dog 'dog))
(format t "floor 5.5 = ~d~%" (floor 5.5))
(format t "ceiling 5.5 = ~d~%" (ceiling 5.5))
(format t "max 5 10 = ~d~%" (max 5 10))
(format t "min 5 10 = ~d~%" (min 5 10))
(format t "oddp 15 = ~d~%" (oddp 15))
(format t "evenp 15 = ~d~%" (evenp 15))
(format t "numberp 2 = ~d~%" (numberp 2))
(format t "null nil = ~d~%" (null nil))
(defun example-calculations ()
  (let* ((angle 30)                       ; Angle in degrees
         (radians (* pi (/ angle 180))) ; Convert to radians
         (vec1 '(1 2 3))                 ; First vector
         (vec2 '(4 5 6))                 ; Second vector
         (matrix1 (make-array '(2 2) :initial-contents '((1 2) (3 4)))) ; 2x2 matrix
         (matrix2 (make-array '(2 2) :initial-contents '((5 6) (7 8))))) ; Another 2x2 matrix

    ;; Basic Arithmetic
    (format t "Arithmetic Operations:~%")
    (format t "5 + 3 = ~d~%" (+ 5 3))
    (format t "5 - 3 = ~d~%" (- 5 3))
    (format t "5 * 3 = ~d~%" (* 5 3))
    (format t "5 / 3 = ~f~%" (/ 5 3))
    (format t "Exponential (2^3) = ~d~%" (expt 2 3))
    (format t "Square root of 81 = ~f~%" (sqrt 81))
    
    ;; Logarithmic Functions
    (format t "~%Logarithmic Functions:~%")
    (format t "Log(100) = ~f~%" (log 100))
    (format t "Log10(100) = ~f~%" (log 10 100)) ; Check your implementation

    ;; Trigonometric Functions
    (format t "~%Trigonometric Functions:~%")
    (format t "Sin(30 degrees) = ~f~%" (sin radians))
    (format t "Cos(30 degrees) = ~f~%" (cos radians))
    (format t "Tan(30 degrees) = ~f~%" (tan radians))
    
    ;; Vector Operations
    (format t "~%Vector Operations:~%")
    (format t "Dot product of vec1 and vec2 = ~d~%" 
            (reduce #'+ (mapcar #'* vec1 vec2))) ; Manual dot product

    ;; Matrix Operations
    (format t "~%Matrix Operations:~%")
    (format t "Matrix1: ~a~%" matrix1)
    (format t "Matrix2: ~a~%" matrix2)
    
    ;; Transpose (manual)
    (let ((transpose (make-array (array-dimensions matrix1) :initial-element 0)))
      (dotimes (i 2)
        (dotimes (j 2)
          (setf (aref transpose j i) (aref matrix1 i j))))
      (format t "Transpose of Matrix1: ~a~%" transpose))
    
    ;; Determinant (manual for 2x2)
    (format t "Determinant of Matrix1 = ~d~%" 
            (- (* (aref matrix1 0 0) (aref matrix1 1 1))
               (* (aref matrix1 0 1) (aref matrix1 1 0))))))

;; Call the function to run calculations
(example-calculations)
