
;; ackermann.lisp

(defun ackermann (n m &optional (depth 0))
  (let* ((indent (make-string (* depth 2) :initial-element #\Space))
         (call-str (format nil "~aAckermann(~d, ~d)" indent n m)))
    (format t "~a [Call]~%" call-str)
    (let ((start-time (get-internal-real-time)))
      (prog1
          (cond
            ((= n 0) 
             (let ((result (1+ m)))
               (format t "~a [Return ~d] (Base case n=0)~%" call-str result)
               result))
            ((and (> n 0) (= m 0))
             (let ((result (ackermann (1- n) 1 (1+ depth))))
               (format t "~a [Return ~d] (Case n>0 and m=0)~%" call-str result)
               result))
            (t 
             (let* ((inner (ackermann n (1- m) (1+ depth)))
                    (result (ackermann (1- n) inner (1+ depth))))
               (format t "~a [Return ~d] (Recursive case)~%" call-str result)
               result)))
        (let ((elapsed (/ (- (get-internal-real-time) start-time)
                          internal-time-units-per-second)))
          (format t "~a [Time ~,3f seconds]~%" call-str elapsed))))))

(defun timed-ackermann (n m)
  (format t "~%Computing Ackermann(~d, ~d)~%" n m)
  (let ((start (get-internal-real-time)))
    (let ((result (ackermann n m 0)))
      (let ((elapsed (/ (- (get-internal-real-time) start)
                       internal-time-units-per-second)))
        (format t "~%Final result: ~d~%" result)
        (format t "Total execution time: ~,3f seconds~%" elapsed)
        result))))

;; Test call
(format t "~%Result of (ackermann 1 1): ~d~%" (ackermann 1 1))
(ackermann 4 2)
