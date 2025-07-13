(print "What's your name?")
(defvar *name* (read))

(defun hello-you (name)
  (format t "Hello, ~o!~%" name))

(setq *print-case* :capitalize)

(hello-you *name*)
