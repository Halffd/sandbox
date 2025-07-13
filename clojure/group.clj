
(ns program)

(def products [{:name "Laptop"
                :category "Electronics"
                :price 999.99M}
               {:name "Smartphone"
                :category "Electronics"
                :price 499.99M}
               {:name "T-Shirt"
                :category "Clothing"
                :price 19.99M}
               {:name "Jeans"
                :category "Clothing"
                :price 39.99M}
               {:name "Coffee Maker"
                :category "Home Appliances"
                :price 89.99M}])

(def grouped-products (group-by :category products))

(defn -main []
  (doseq [group grouped-products]
    (println (str "Category: " (first group)))
    (doseq [product (last group)]
      (println (str " - " (:name product) ": " (:price product))))))