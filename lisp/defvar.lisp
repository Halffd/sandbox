;; Set the print case to capitalize
(setq *print-case* :capitalize)

;; Define a variable
(defvar *number* 0)

;; Set the number
(setf *number* 6)

;; Function to demonstrate various format specifications
(defun format-examples ()
  (format t "Number with commas: ~,d~%" 100000000)   ; Integer with commas
  (format t "PI to 5 characters: ~5f~%" 3.141593)     ; Floating-point to 5 characters
  (format t "PI to 4 decimals: ~4f~%" 3.141593)       ; Floating-point to 4 decimal places
  (format t "10 Percent: ~,2f~%" 0.10)                 ; Percentage with 2 decimal places
  (format t "10 Dollars: ~$~2f~%" 10.00)               ; Currency format with 2 decimal places
  (format t "Current number is: ~d~%" *number*))        ; Output the variable *number*

;; Run the format examples function
(format-examples)