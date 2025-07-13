(defun factorial (n &optional (acc 1))
  (if (zerop n) acc
      (factorial (1- n) (* acc n))))

(print (factorial 5))