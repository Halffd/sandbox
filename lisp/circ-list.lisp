;; First, make sure we have circular printing enabled
(setf *print-circle* t)

;; Try a simpler circular list creation
(defparameter *my-circular-list* (list 1 2 3))
(setf (cdddr *my-circular-list*) *my-circular-list*)

;; Check what we've got
(print *my-circular-list*)

;; Try accessing elements safely
(car *my-circular-list*)
(cadr *my-circular-list*)
(cdr *my-circular-list*)
