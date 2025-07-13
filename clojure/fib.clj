(defn fib
  ([] (fib 0))
  ([n] (fib n [1 0]))
  ([n list]
   (cond
     (= n 1) (reverse list)
     :else (let [[a b & _] list]
             (fib (dec n) (cons (+ a b) list))))))

(fib 14)
