(ns encode-binary.core
  (:import java.nio.ByteBuffer)
  (:import java.nio.charset.Charset)
  (:import java.nio.charset.StandardCharsets)
  (:import java.nio.ByteOrder)
  (:require [clojure.spec-alpha2 :as s]
            [clojure.set :as set]))

(defn- deep-resolve [reg k]
  (loop [spec k]
    (if (ident? spec)
      (recur (get reg spec))
      spec)))

(defn- reg-resolve
  "returns the spec/regex at end of alias chain starting with k, nil if not found, k if k not ident"
  [k]
  (if (ident? k)
    (let [reg (s/registry)
          spec (get reg k)]
      (if-not (ident? spec)
        spec
        (deep-resolve reg spec)))
    k))

(defn- reg-resolve!
  "returns the spec/regex at end of alias chain starting with k, throws if not found, k if k not ident"
  [k]
  (if (ident? k)
    (or (reg-resolve k)
          (throw (Exception. (str "Unable to resolve spec: " k))))
    k))

(defn- specize [x]
    (if (keyword? x) (reg-resolve! x) x))

(defprotocol Binary
  :extend-via-metadata true
  (alignment* [this])
  (sizeof* [this])

(defprotocol Codec
  :extend-via-metadata true
  (encode* [this data])
  (decode* [this binary-seq]))

(defn codec? [maybe-c])

(defn codify [x enc dec & {:keys [alignment size]
                           :or {alignment nil size nil}}]
  (with-meta (s/spec x) 
             (assoc (meta x)
                    `encode* enc
                    `decode* dec
                    `alignment* (constantly alignment)
                    `sizeof* (constantly size))))
(defn specify [])

(defn alignment [bin]
  (alignment* (specize bin)))

(defn sizeof [bin]
  (sizeof* (specize bin)))

(defn encode [codec data]
  (encode* (specize codec) data))

(defn decode [codec binary-seq]
  (decode* (specize codec) binary-seq))


(defn -main []
  (println "Hello World"))
