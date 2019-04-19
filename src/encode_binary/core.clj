(ns encode-binary.core
  (:import java.nio.ByteBuffer)
  (:import java.nio.charset.Charset)
  (:import java.nio.charset.StandardCharsets)
  (:import java.nio.ByteOrder)
  (:require [clojure.spec-alpha2 :as s]
            [clojure.set :as set]))

(defprotocol Binary
  :extend-via-metadata true
  (alignment* [this] "The alignment of this binary.  The resulting value will conform to :encode-binary/alignment")
  (sizeof* [this] "The size in bytes of the binary.  If the size cannot be determined with the current value, then it will return nil"))

(defprotocol Codec
  :extend-via-metadata true
  (encode* [this data])
  (decode* [this binary-seq]))

(defn codify [])
(defn specify [])

(defn foo [a b] (+ a b))

(defn -main []
  (println "Hello World"))
