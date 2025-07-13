(defun compute (a n)
  "Compute product of (i+j) for i from 1 to n and j from 0 to n-1."
  (let ((start-time (current-time)))
  (let ((i 1))
    (while (<= i n)
      (dotimes (j n)  ; Inner loop runs n times (0 to n-1)
        (setq a (* a (+ i j))))
      (setq i (1+ i))))
  
  (let* ((end-time (current-time))
         (duration (float-time (time-subtract end-time start-time))))
    (message "Result: %s\nTime elapsed: %.6f seconds" a duration)
    a))

;; Example usage:
(compute 1 5)  ; Calculate for n=5 with initial a=1
(defun soma (a b)
  "Soma os números a e b de forma O(n), onde n = max(a, b)."
  (let ((n (max a b))
        (sum 0))
    (dotimes (_ n sum) ;; Executa n vezes
      (setq sum (+ sum 1))))) ;; Operação constante dentro do loop
(setq a (1+ a)) ;; O(1)
(setq resultado (soma a 10)) ;; O(n)

(message "Resultado: %d" resultado) ;; Exibe o resultado
