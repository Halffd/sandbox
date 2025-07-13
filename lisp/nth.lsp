(nth 100 '(1 2)) ;; no error
;; (defun x (nth 100 '(1 2)))
(defun x (lst) (nth 100 lst))
(x '(1 2))  ; This will still return NIL(nth 100 '(1 2)) ;; no error
(defun log-message (message)
  "Log a message to the console."
  (format t "~A~%" message))

(defun x (lst)
  "Return the 100th element of the list, logging the action."
  (let ((index 100))
    (if (>= (length lst) (1+ index))  ; Check if the list is long enough
        (progn
          (log-message (format nil "Accessing index ~A of list: ~A" index lst))
          (nth index lst))
        (progn
          (log-message (format nil "Index ~A is out of bounds for list: ~A" index lst))
          nil))))  ; Return NIL if out of bounds

;; Example usage
(format t "Result: ~A~%" (x '(1 2)))  ; This will log the error and return NIL
(format t "Result: ~A~%" (x '(1 2 3 4 5)))  ; This will log access and return NIL
