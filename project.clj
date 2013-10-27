(defproject clojure-btree "0.1.0-SNAPSHOT"
  :description "Binary Search Tree implemented in Clojure"
  :url "https://github.com/baumandm/clojure-btree"
  :license {:name "Unlicense"
            :url "http://unlicense.org/"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :main ^:skip-aot clojure-btree.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
