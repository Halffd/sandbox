;; Basic square root implementation (as before)
(defn good-enough? [guess x]
  (> 0.001 (Math/abs (- guess (/ x guess)))))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve-guess [guess x]
  (average guess (/ x guess)))

(defn sqrt [x]
  (letfn [(sqrt-iter [guess]
            (if (good-enough? guess x)
              guess
              (sqrt-iter (improve-guess guess x))))]
    (sqrt-iter 1.0)))

;; Java interop examples

;; Run garbage collection
(defn force-gc []
  (System/gc)
  (println "Garbage collection requested"))

;; Get free memory in MB
(defn free-memory []
  (let [free-mem (/ (.freeMemory (Runtime/getRuntime)) 1024.0 1024.0)]
    (format "Free memory: %.2f MB" free-mem)))

;; Get max memory in MB
(defn max-memory []
  (let [max-mem (/ (.maxMemory (Runtime/getRuntime)) 1024.0 1024.0)]
    (format "Maximum memory: %.2f MB" max-mem)))

;; Memory statistics
(defn memory-stats []
  (println (free-memory))
  (println (max-memory)))

;; Character.toLowerCase example
(defn to-lowercase [s]
  (apply str (map #(Character/toLowerCase %) s)))

;; Examples
(comment
  ;; Run these in your REPL
  (System/out/print 'hello world)
  ;; Trigger garbage collection
  (force-gc)
  
  ;; Check memory before and after GC
  (memory-stats)
  (force-gc)
  (memory-stats)
  
  ;; Test character conversion
  (to-lowercase "HELLO CLOJURE!")  ;; => "hello clojure!"
  (to-lowercase "Mixed CASE String")  ;; => "mixed case string"
  
  ;; Measure memory consumption of a large operation
  (memory-stats)
  (def large-vec (vec (range 1000000)))
  (memory-stats)
  (force-gc)
  (memory-stats)
  
  ;; Combining with our sqrt function
  (sqrt 16)  ;; => 4.000000636692939
  (to-lowercase (str "The SQUARE ROOT of 16 is " (sqrt 16)))
  ;; => "the square root of 16 is 4.000000636692939"
)
