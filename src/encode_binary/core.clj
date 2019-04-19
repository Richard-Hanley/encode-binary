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

(def default-order ByteOrder/LITTLE_ENDIAN)
(def order-map {:little ByteOrder/LITTLE_ENDIAN
                :big ByteOrder/BIG_ENDIAN
                :native (ByteOrder/nativeOrder)
                :network ByteOrder/BIG_ENDIAN})
(defprotocol Binary
  :extend-via-metadata true
  (alignment* [this])
  (sizeof* [this]))

(defprotocol Codec
  :extend-via-metadata true
  (encode* [this data])
  (decode* [this binary-seq]))

(defn codify [x enc dec & {:keys [alignment size]
                           :or {alignment nil size nil}}]
  (with-meta (if (s/spec? x)
               x
               (s/spec x))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Defining Primitive Codecs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def primitive-ops
  {Byte {:get-buffer #(.get %) :put-buffer #(.put %1 (unchecked-byte %2))}
   Short {:get-buffer #(.getShort %) :put-buffer #(.putShort %1 (unchecked-short %2))}
   Integer {:get-buffer #(.getInt %) :put-buffer #(.putInt %1 (unchecked-int %2))}
   Long {:get-buffer #(.getLong %) :put-buffer #(.putLong %1 (unchecked-long %2))}
   Float {:get-buffer #(.getFloat %) :put-buffer #(.putFloat %1 (unchecked-float %2))}
   Double {:get-buffer #(.getDouble %) :put-buffer #(.putDouble %1 (unchecked-double %2))}})



(defn signed-primitive-spec [min-value max-value]
  `(s/and int? 
          #(<= ~min-value % ~max-value)))

(defn floating-primitive-spec [min-value max-value]
  `#(or (zero? %)
        (<= ~min-value % ~max-value)))

; (defmacro unsigned-primitive-spec [class]

; (defn prim-size [conv] `(. ~(type (conv 0)) ~'SIZE))

; (defmacro put-prim [put conv order]
;   `(fn [buffer value] (. buffer (~conv value)gt

(defn put-prim [put-buffer size order]
  (fn [_ data]
    (let [buff (.order (ByteBuffer/allocate size)
                       (get order-map order default-order))
          _ (put-buffer buff (or data 0))]
      (seq (.array buff)))))

(defn get-prim [get-buffer size order]
  (fn [_ binary-seq]
    (let [[prim remaining] (split-at size binary-seq)
          bytes (.order (ByteBuffer/wrap (byte-array prim))
                        (get order-map order default-order))]
      [(get-buffer bytes) remaining])))

(defmacro primitive [prim & {:keys [word-size order]
                             :or {word-size 1}}]
  (let [size `(. ~prim ~'BYTES)
        min-value `(. ~prim ~'MIN_VALUE)
        max-value `(. ~prim ~'MAX_VALUE)
        ops `(get primitive-ops ~prim)]
    `(codify (s/spec* (signed-primitive-spec ~min-value ~max-value))
             (put-prim (:put-buffer ~ops) ~size ~order)
             (get-prim (:get-buffer ~ops) ~size ~order)
             :alignment (min ~size ~word-size)
             :size ~size)))

(defmacro unsigned-primitive [prim & {:keys [word-size order]
                             :or {word-size 1}}]
  (let [size `(. ~prim ~'BYTES)
        max-value `(bit-shift-left 1 (. ~prim ~'SIZE))
        to-unsigned `#(. ~prim toUnsignedLong %)
        ops `(get primitive-ops ~prim)]
    `(codify (s/int-in 0 ~max-value)
             (put-prim (:put-buffer ~ops) ~size ~order)
             (get-prim (comp ~to-unsigned (:get-buffer ~ops)) ~size ~order)
             :alignment (min ~size ~word-size)
             :size ~size)))

(defmacro floating-primitive [prim & {:keys [word-size order]
                             :or {word-size 1}}]
  (let [size `(. ~prim ~'BYTES)
        min-value `(. ~prim ~'MIN_VALUE)
        max-value `(. ~prim ~'MAX_VALUE)
        ops `(get primitive-ops ~prim)]
    `(codify (s/spec* (floating-primitive-spec ~min-value ~max-value))
             (put-prim (:put-buffer ~ops) ~size ~order)
             (get-prim (:get-buffer ~ops) ~size ~order)
             :alignment (min ~size ~word-size)
             :size ~size)))

(s/def ::int8 (primitive Byte))
(s/def ::int16 (primitive Short))
(s/def ::int32 (primitive Integer))
(s/def ::int64 (primitive Long))

(s/def ::uint8 (unsigned-primitive Byte))
(s/def ::uint16 (unsigned-primitive Short))
(s/def ::uint32 (unsigned-primitive Integer))

(s/def ::float (floating-primitive Float))
(s/def ::double (floating-primitive Double))

