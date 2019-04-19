(ns encode-binary.core-test
    (:require [clojure.test :refer :all]
              [clojure.spec.alpha :as s]
              [encode-binary.core :as e]))

(deftest addition
  (is (= 4 (+ 2 2)))
  (is (= 5 (+ 2 2))))
