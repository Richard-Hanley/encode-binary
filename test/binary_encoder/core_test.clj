(ns binary-encoder.core-test
    (:require [clojure.test :refer :all]
              [clojure.spec.alpha :as s]
              [binary-encoder.core :as bin]))

(deftest addition
  (is (= 4 (+ 2 2)))
  (is (= 5 (+ 2 2))))
