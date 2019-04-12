(ns binary-encoder.core
  (:import java.nio.ByteBuffer)
  (:import java.nio.charset.Charset)
  (:import java.nio.charset.StandardCharsets)
  (:import java.nio.ByteOrder)
  (:require [clojure.spec-alpha2 :as s]
            [clojure.set :as set]))

(defn foo [a b] (+ a b))

(defn -main []
  (println "Hello World"))
