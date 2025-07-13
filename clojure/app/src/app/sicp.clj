(ns app.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn -main
  [& args]
  ;; Move all function definitions outside of -main or use letfn
  
  ;; Filtering a sequence to select only those elements that satisfy a given predicate
  (letfn [(filter-seq [predicate sequence]
            (cond
              (empty? sequence) nil
              (predicate (first sequence))
              (cons (first sequence)
                    (filter-seq predicate (rest sequence)))
              :else (filter-seq predicate (rest sequence))))

          ;; Accumulations can be implemented by
          (accumulate [op initial sequence]
            (if (empty? sequence)
              initial
              (op (first sequence)
                  (accumulate op initial (rest sequence)))))

          (enumerate-interval [low high]
            (if (> low high)
              nil
              (cons low (enumerate-interval (+ low 1) high))))

          ;; To enumerate the leaves of a tree
          (enumerate-tree [tree]
  (cond
    (nil? tree) nil
    (not (coll? tree)) (list tree)
    (empty? tree) nil  ; Add this line
    :else (concat (enumerate-tree (first tree))
                  (enumerate-tree (rest tree)))))

          (fib [n]
            (cond
              (= n 0) 0
              (= n 1) 1
              :else (+ (fib (- n 1)) (fib (- n 2)))))

          (sum-odd-squares [tree]
            (accumulate +
                        0
                        (map #(* % %)
                             (filter odd?
                                     (enumerate-tree tree)))))

          (even-fibs [n]
            (accumulate cons
                        nil
                        (filter even?
                                (map fib
                                     (enumerate-interval 0 n)))))

          (list-fib-squares [n]
            (accumulate cons
                        nil
                        (map #(* % %)
                             (map fib
                                  (enumerate-interval 0 n))))

            (product-of-squares-of-odd-elements [sequence]
                                                (accumulate *
                                                            1
                                                            (map #(* % %)
                                                                 (filter odd? sequence)))))
          

          (salary [record]
            (:salary record))

          (programmer? [record]
            (= (:role record) "programmer"))

          (salary-of-highest-paid-programmer [records]
            (accumulate max
                        0
                        (map salary
                             (filter programmer? records))))]

    ;; Now run your examples
    (println "Filter example:" (filter-seq odd? [1 2 3 4 5]))
    (println "Accumulate example:" (accumulate + 0 [1 2 3 4 5]))
    (println "Enumerate interval:" (enumerate-interval 2 7))
    (println "Enumerate tree:" (enumerate-tree [1 [2 [3 4]] 5]))
    (println "List fib squares:" (list-fib-squares 10))
    (println "Product of squares:" (product-of-squares-of-odd-elements [1 2 3 4 5]))
    
    (let [records [{:name "Alice" :role "programmer" :salary 90000}
                   {:name "Bob" :role "designer" :salary 70000}
                   {:name "Carol" :role "programmer" :salary 95000}
                   {:name "Dave" :role "manager" :salary 85000}]]
      (println "Highest paid programmer:" (salary-of-highest-paid-programmer records))))
	(map #(+ % 1) [1 2 3 8 9 0])
(map #(str % "!") ["a" "hi" "b" "x"])
(println (format "%s" (str "a \t" "b" "\nc")))
  ;; -> ("a!" "hi!" "b!" "x!")
  (filter #(> % 5) [1 9 4 -1 46 5])
  )
