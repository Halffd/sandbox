(defun search-ch (tab ch n)
  "Searches for 'ch' in array 'tab' of length 'n' using 1-based indexing."
  (if (<= n 0)
      (values nil nil)
      (let ((i 0)
            (achou nil)
            pos)
        (loop
          do (incf i)
             (when (and (<= i n)
                        (equal (aref tab (1- i)) ch))
               (setf achou t))
          until (or achou (= i n)))
        (when achou (setf pos i))
        (values achou pos))))

;; Demonstration examples with formatted output
(format t "~%~%=== Basic Search Examples ===~%~%")

(multiple-value-bind (found pos)
    (search-ch #(5 3 7 1 9) 7 5)
  (format t "1. Search for 7 in [5,3,7,1,9]~%   Found: ~a, Position: ~a~%~%" found pos))

(multiple-value-bind (found pos)
    (search-ch #(5 3 7 1 9) 4 5)
  (format t "2. Search for 4 in [5,3,7,1,9]~%   Found: ~a, Position: ~a~%~%" found pos))

(multiple-value-bind (found pos)
    (search-ch #(5 3 7 1 9) 9 5)
  (format t "3. Search for 9 (last element)~%   Found: ~a, Position: ~a~%~%" found pos))

(multiple-value-bind (found pos)
    (search-ch #(5) 5 1)
  (format t "4. Single-element array (match)~%   Found: ~a, Position: ~a~%~%" found pos))

(multiple-value-bind (found pos)
    (search-ch #() 5 0)
  (format t "5. Empty array test~%   Found: ~a, Position: ~a~%~%" found pos))

(multiple-value-bind (found pos)
    (search-ch #(10 20 30) 10 3)
  (format t "6. Search first element~%   Found: ~a, Position: ~a~%~%" found pos))

(format t "=== End of Examples ===~%~%")
