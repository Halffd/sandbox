;; Basic System.out.println in Clojure
(. System/out println "hello world")

;; Alternative doto syntax
(doto System/out 
  (.println "hello world"))

;; The slick, modern way
(import '(java.io PrintStream))
(System/out println "hello world")

;; Even more idiomatic
(println "hello world")  ; This actually uses Java's println under the hood anyway
(format "hello world!")
