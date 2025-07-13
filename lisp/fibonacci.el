(defun fibonacci (n)
  "Return the Nth Fibonacci number recursively.
The sequence is defined with F(0)=0, F(1)=1."
  (if (<= n 1)
      n
    (+ (fibonacci (- n 1))
       (fibonacci (- n 2)))))
(defun fibonacci-iter (n)
  "Return the Nth Fibonacci number using an iterative approach.
The sequence is defined with F(0)=0, F(1)=1."
  (let ((a 0)
        (b 1))
    (dotimes (i n a)
      (let ((temp a))
        (setq a b
              b (+ temp b))))))
(defun fibonacci-sum (n)
  "Return the sum of Fibonacci numbers from F(0) to F(n) inclusive.
Uses the identity: sum = F(n+2) - 1, where F(0)=0 and F(1)=1."
  (if (< n 0)
      (error "n must be non-negative")
    (- (fibonacci-iter (+ n 2)) 1)))

(fibonacci 111)      ; => 55
(fibonacci-iter 10) ; => 55
(fibonacci-sum 8)
