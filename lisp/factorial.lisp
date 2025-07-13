(defun fatorial (n)
  (let ((aux 1))
    (format t "Calculating factorial of ~a~%" n)
    (loop for i from 1 to n do
      (setf aux (* aux i))
      (format t "Step ~a: Multiply by ~a → Result: ~a~%" i i aux))
    aux))
(fatorial 12)
