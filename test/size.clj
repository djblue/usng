(ns size
  (:require [clojure.test :refer [deftest is]]))

(deftest bundle-size
  (is (< (count (slurp "target/usng/index.js")) 20000)))
