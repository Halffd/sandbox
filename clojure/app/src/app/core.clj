(ns app.core
  (:require [clojure.string :as str])
  (:gen-class))


(defn celsius->fahrenheit [celsius]
 (double (+ 32 (* 9/5 celsius))))
(defn celsius->fahrenheit [celsius]
  (double (+ 32 (* 9/5 celsius))))

(defn fahrenheit->celsius [fahrenheit]
  (double (* 5/9 (- fahrenheit 32))))

(defn fahrenheit->kelvin [fahrenheit]
  (double (+ 273.15 (* 5/9 (- fahrenheit 32)))))

(defn celsius->joule [celsius mass]
  ;; Using specific heat capacity of water (4184 J/kg·°C)
  ;; Energy = mass × specific_heat × temperature_change
  ;; Assuming temperature change from 0°C
  (double (* mass 4184 celsius)))
(defn -main
  [& args]
  (println "Hello, World!")
  (let [v 9
        result-str (format "v = %02d %-5d %.3f" v (* v v) (double (/ v 20)))]
    (println "Type of v:" (type v))
    (println "Nil?" (nil? v))
    (println "Tests:" (and (pos? v) (not (even? v)) (odd? v) (not (neg? v)) (number? v) (not (float? v))))
    (println "Type of false:" (type false))
    (println "Blank?" (str/blank? result-str))
    (println "Includes 9?" (str/includes? result-str "9"))
    (println "Capitalized:" (str/capitalize result-str))
    (println "Index of '9':" (str/index-of result-str "9"))
    (println "Join:" (str/join "-" [result-str v]))
    (println "Replace:" (str/replace result-str "0" "X"))
    (println "Reversed:" (str/reverse result-str))
    (println "Processed:" (str/trim (str/lower-case result-str)))
    
    ;; Fixed the broken parts:
    (let [split-result (str/split result-str #" ")
          digit-splits (str/split result-str #"\d")
          user-input (do (print "Sum: ") (flush) (read-line))
          parsed-input (Integer/parseInt user-input)]
      (println "Split by spaces:" split-result)
      (println "Split by digits:" digit-splits)
      (println "You entered:" user-input)
      (println "Sum with each split:" 
               (map #(+ (count %) parsed-input) split-result))))

  (print "Celsius: ")
  (flush)
  (->> (read-line)
       parse-double
       celsius->fahrenheit
       (println "Fahrenheit:"))
(print "F: ")
  (flush)
  (->> (read-line)
       parse-double
       fahrenheit->celsius
       (println "Celsius:"))
(->> [1 2 3 4 5]
     (map inc)           ; [2 3 4 5 6]
     (filter even?)      ; [2 4 6]  
     (take 2)           ; [2 4]
     (reduce +))        ; 6
(-> [1 2 3 4 5 9 -4 11 18]
    (->> (map #(Math/sqrt (Math/abs %)))))
(-> [1 2 3 4 5 9 -4 11 18]
    (->> (map #(Math/sqrt (Math/abs %)))
         (filter #(odd? (int %)))))
(-> [1 2 3 4 5 9 -4 11 18]
    (->> (map #(Math/sqrt (Math/abs %)))
         (filter #(odd? (int %)))
         (take 2)))
(-> [1 2 3 4 5 9 -4 11 18]
    (->> (map #(Math/sqrt (Math/abs %)))
         (filter #(odd? (int %)))
         (take 2)
         (reduce +)))
(reduce / (take 3 (filter even? (map inc [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 -2]))))
(-> "hello"
    (str " world")      ; (str "hello" " world")
    (clojure.string/upper-case)
    (clojure.string/reverse))
(->> [1 2 3 4 5]
     (reduce + 10))      ; sum with initial value 10

(->> [1 2 3 4 5]
     (reduce * 1))      ; product with initial value 1

(->> ["a" "b" "c"]
     (reduce str "start: "))  ; "start: abc"
(->> [1 2 3]
     (reduce #(Math/pow %1 %2) 20))  ; 20^1^2^3 = 20^1 = 20, then 20^2 = 400, then 400^3 = 64000000
)
