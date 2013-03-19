(ns task01.core
  (:require [pl.danieljanus.tagsoup :refer :all])
  (:gen-class))

(defn first-attribute [node] (get node 2))

(defn get-links []
  " 1) Find all elements containing {:class \"r\"}.

  Example:
  [:h3 {:class \"r\"} [:a {:shape \"rect\", :class \"l\",
                           :href \"https://github.com/clojure/clojure\",
                           :onmousedown \"return rwt(this,'','','','4','AFQjCNFlSngH8Q4cB8TMqb710dD6ZkDSJg','','0CFYQFjAD','','',event)\"}
                       [:em {} \"clojure\"] \"/\" [:em {} \"clojure\"] \" · GitHub\"]]

     2) Extract href from the element :a.

  The link from the example above is 'https://github.com/clojure/clojure'.

    3) Return vector of all 10 links.

  Example: ['https://github.com/clojure/clojure', 'http://clojure.com/', . . .]
  "
  (let [data (parse "clojure_google.html")]
    (defn search-link?[vector]
      (= (:class (attributes vector)) "r"))
    (defn href[vector]
      (:href (attributes (first-attribute vector))))
    (defn elements[html-source]
      (let [vectors (filter vector? html-source)]
        (remove empty?
          (concat (map elements (remove search-link? vectors))
            (map href (filter search-link? vectors))))))
    (apply vector (flatten (elements data)))))

(defn -main []
  (println (str "Found " (count (get-links)) " links!")))
