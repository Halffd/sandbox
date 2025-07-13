;; Define a function that outputs messages about various groups
(defun print-messages-about-groups ()
  "Print emphatic messages about various groups in uppercase."
  ;; Create our list of targets
  (let ((targets '("s" "s" "t s")))
    ;; Loop through each target and print a message
    (dolist (target targets)
      ;; Format the message in uppercase
      (format t "Shoowing ~A" (string-upcase target))
      ;; Add a newline
      (terpri)
      ;; Ensure output is displayed immediately
      (force-output))))

;; Call the function
(print-messages-about-groups)


(let ((list (list (list (list (list "jam") (list "carles") (list "russ"))))))(let ((list (apply #'append (list (list (car (caaaar (list list)))) (list (car (cadr (caaar (list list)))))(car (list (nth 1 (cdr (caar list))))) (car (list (nth 2 (caadr list))))))))`(progn ,@(map 'list (lambda (list) (format t (string-upcase (format nil "Hello ~A " list))) (terpri) (force-output)) list))))

(defmacro ->> (var &rest lst)
 (if (car lst)
 `(->> (,@(car lst) ,var) ,@(cdr lst)) 
 `,var))
