(setq *print-case* :capitalize)

(defparameter *name* 'Half)

(format t "(eq *name* 'Half) = ~d ~%" (eq *name* 'Half))

(defparameter *name* 'Half)

(format t "(equal 'car 'truck) = ~d ~%" (equal 'car 'truck))
(format t "(equal 'car 'car) = ~d ~%" (equal 'car 'car))
(format t "(equal 10 10) = ~d ~%" (equal 10 10))
(format t "(equal 5.5 5.3) = ~d ~%" (equal 5.5 5.3))
(format t "(equal 5.5 5.5) = ~d ~%" (equal 5.5 5.5))

(format t "(equal \"string\" \"String\") = ~d ~%"
        (equal "string" "String"))
(format t "(equal \"String\" \"String\") = ~d ~%"
        (equal "String" "String"))

(format t "(equal (list 1 2 3) (list 1 2 3)) = ~d ~%"
        (equal (list 1 2 3) (list 1 2 3)))
