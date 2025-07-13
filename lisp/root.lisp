(defun raiz (x r tol)
  (if (<= (abs (- x (* r r))) tol)
      r
      (raiz x (/ (+ (/ x r) r) 2) tol)))
(raiz 10000 1.0 0.0001)

(setq x (1- (expt 2 1024)))
(incf x)  ;; x becomes 2^32, no overflow
;; Should return something close to 3
