

(defpackage :beyblade-physics
  (:use :cl :sb-ext))

(in-package :beyblade-physics)

;;; Load numerical libraries (using GSLL - GNU Scientific Library for Lisp)
;;; You'll need to install these first with Quicklisp
(ql:quickload '(:gsll :cl-plplot))

;;; Define our physical constants
(defparameter *alpha* 0.5 "Paraboloid steepness")
(defparameter *g* 9.81 "Gravity's vendetta against fun")

;;; Get user input for friction with appropriate snark
(format t "~%=== BEYBLADE PHYSICS SIMULATOR 9000: LISP EDITION ===~%")
(format t "Input friction coefficient (μ):~%")
(format t " • 0.0: Frictionless fantasy land (parentheses forever)~%")
(format t " • 0.05-0.1: Decent Beyblade on smooth surface~%")
(format t " • 0.2-0.5: Did you lubricate with maple syrup?~%")
(format t " • 1.0+: Basically spinning in concrete~%")

(defun get-mu ()
  "Gets μ from user with error checking and sarcasm"
  (format t "Enter μ value (recommended: 0.0-1.0): ")
  (finish-output)
  (let ((input (read)))
    (if (and (numberp input) (>= input 0))
        input
        (progn 
          (format t "That's not a valid number. Did you forget to close a parenthesis in your life?~%")
          (get-mu)))))

(defparameter *mu* (get-mu))

;;; Define the differential equations system function
(defun beyblade-derivative (state time)
  "Returns the derivatives for the Beyblade system"
  (let* ((x (aref state 0))
         (y (aref state 1))
         (vx (aref state 2))
         (vy (aref state 3))
         ;; Surface gradient
         (dz-dx (* 2 *alpha* x))
         (dz-dy (* 2 *alpha* y))
         ;; Denominator for projection
         (denom (sqrt (+ 1 (expt dz-dx 2) (expt dz-dy 2))))
         ;; Acceleration
         (ax (- (* (- *g*) (/ dz-dx denom)) (* *mu* vx)))
         (ay (- (* (- *g*) (/ dz-dy denom)) (* *mu* vy))))
    ;; Return the state derivative vector
    (vector vx vy ax ay)))

;;; Initialize our state vector
(defparameter *initial-state* (vector 0.8 0.0 0.0 0.8))

;;; Setting up the simulation time
(defparameter *sim-time* 
  (if (> *mu* 0)
      (min 50 (max 10 (/ 20 *mu*)))
      20))

;;; Create time points for evaluation
(defparameter *time-steps* 1000)
(defparameter *time-vector* 
  (let ((vec (make-array *time-steps*)))
    (dotimes (i *time-steps*)
      (setf (aref vec i) (* i (/ *sim-time* (1- *time-steps*)))))
    vec))

;;; Helper function for 4th order Runge-Kutta integration
(defun rk4-step (func state time dt)
  "Performs one step of RK4 integration"
  (let* ((k1 (funcall func state time))
         (k2 (funcall func (map 'vector #'+ state (map 'vector (lambda (x) (* x (/ dt 2))) k1))
                      (+ time (/ dt 2))))
         (k3 (funcall func (map 'vector #'+ state (map 'vector (lambda (x) (* x (/ dt 2))) k2))
                      (+ time (/ dt 2))))
         (k4 (funcall func (map 'vector #'+ state (map 'vector (lambda (x) (* x dt)) k3))
                      (+ time dt))))
    (map 'vector #'+ state
         (map 'vector (lambda (a b c d) (* (/ dt 6) (+ a (* 2 b) (* 2 c) d)))
              k1 k2 k3 k4))))

;;; Perform the integration
(defun solve-beyblade-motion ()
  "Solves the Beyblade motion equations using RK4"
  (let* ((result (make-array (list *time-steps* 4)))
         (state (copy-seq *initial-state*))
         (dt (/ *sim-time* (1- *time-steps*))))
    
    ;; Store initial state
    (dotimes (i 4)
      (setf (aref result 0 i) (aref state i)))
    
    ;; Integrate step by step
    (dotimes (i (1- *time-steps*))
      (setf state (rk4-step #'beyblade-derivative state (aref *time-vector* i) dt))
      (dotimes (j 4)
        (setf (aref result (1+ i) j) (aref state j))))
    
    result))

;;; Solve the system
(defparameter *solution* (solve-beyblade-motion))

;;; Calculate z values from the paraboloid equation
(defun calculate-z-values (solution)
  "Calculates z values from x and y positions"
  (let* ((time-steps (array-dimension solution 0))
         (z-values (make-array time-steps)))
    (dotimes (i time-steps)
      (let ((x (aref solution i 0))
            (y (aref solution i 1)))
        (setf (aref z-values i) (* *alpha* (+ (expt x 2) (expt y 2))))))
    z-values))

(defparameter *z-values* (calculate-z-values *solution*))

;;; Calculate final velocity
(defun calculate-velocity (solution)
  "Calculates velocity magnitude at each time step"
  (let* ((time-steps (array-dimension solution 0))
         (velocities (make-array time-steps)))
    (dotimes (i time-steps)
      (let ((vx (aref solution i 2))
            (vy (aref solution i 3)))
        (setf (aref velocities i) (sqrt (+ (expt vx 2) (expt vy 2))))))
    velocities))

(defparameter *velocities* (calculate-velocity *solution*))

;;; Output the results
(defun print-results ()
  "Prints the simulation results"
  (let* ((final-idx (1- *time-steps*))
         (x0 (aref *initial-state* 0))
         (y0 (aref *initial-state* 1))
         (vx0 (aref *initial-state* 2))
         (vy0 (aref *initial-state* 3))
         (initial-vel (sqrt (+ (expt vx0 2) (expt vy0 2))))
         (final-x (aref *solution* final-idx 0))
         (final-y (aref *solution* final-idx 1))
         (dist-from-center (sqrt (+ (expt final-x 2) (expt final-y 2))))
         (final-vel (aref *velocities* final-idx)))
    
    (format t "~%Simulation Results:~%")
    (format t "• Initial velocity: ~,2F m/s~%" initial-vel)
    (format t "• Final position: (~,3F, ~,3F)~%" final-x final-y)
    (format t "• Distance from center: ~,3F m~%" dist-from-center)
    (format t "• Spiral completed in ~,2F seconds~%" *sim-time*)
    (format t "• ~A~%" (if (> final-vel 0.01) "Still moving!" "Effectively stopped."))))

;;; Try to plot if we have a graphical environment
(defun attempt-plot ()
  "Tries to plot the trajectory using PLplot"
  (handler-case 
      (progn
        (format t "~%Attempting to create plots...~%")
        (let ((x-vals (make-array *time-steps*))
              (y-vals (make-array *time-steps*))
              (z-vals *z-values*))
          (dotimes (i *time-steps*)
            (setf (aref x-vals i) (aref *solution* i 0))
            (setf (aref y-vals i) (aref *solution* i 1)))
          
          ;; This is where we'd call PLplot functions to plot in 3D
          ;; But since that's complex and environment-dependent, we'll just save to a data file
          (with-open-file (stream "beyblade_trajectory.dat" :direction :output :if-exists :supersede)
            (format stream "# Beyblade Trajectory (mu = ~F)~%" *mu*)
            (format stream "# X Y Z~%")
            (dotimes (i *time-steps*)
              (format stream "~F ~F ~F~%" 
                      (aref x-vals i) (aref y-vals i) (aref z-vals i))))
          (format t "Data saved to 'beyblade_trajectory.dat'~%")
          (format t "Plot it using your favorite plotting tool (gnuplot, python, etc.)~%")))
    (error (e)
      (format t "~%Couldn't create plots: ~A~%" e)
      (format t "Data saved to text file instead.~%"))))

;;; Run the simulation
(print-results)
(attempt-plot)

;;; Final message
(format t "~%LISP BEYBLADE PHYSICS: Where parentheses go to die, and indentation goes to cry.~%")