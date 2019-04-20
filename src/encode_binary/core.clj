(ns encode-binary.core
  (:import java.nio.ByteBuffer)
  (:import java.nio.charset.Charset)
  (:import java.nio.charset.StandardCharsets)
  (:import java.nio.ByteOrder)
  (:require [clojure.spec-alpha2 :as s]
            [clojure.set :as set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resolvers from spec registry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binary Collections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn indexed-binary [index binary-coll]
  (with-meta binary-coll
             (assoc (meta binary-coll) ::index index)))

(defn current-index [binary-coll]
  (or (::index (meta binary-coll)) 0))

(defn split-binary [size bin]
  (let [[bin rem] (split-at size bin)]
    [bin (if (some? rem)
           (with-meta rem
                    (update (meta bin) ::index (fnil + 0) size)))]))
  
(defprotocol BinaryCollection
  (flatten [this]))

(defn make-binary 
  [bin & {:keys [key-order alignment]}]
  (with-meta bin {::key-order key-order ::alignment alignment}))

(defn align-binary [bin align-to]
  (with-meta bin
             (assoc (meta bin) ::alignment align-to)))

(defn binary-coll-alignment [bin]
  (or (get (meta bin) ::alignment) 1))

(defn binary-coll-key-order [bin]
  (or (get (meta bin) ::key-order) (keys bin)))

(defn alignment-padding [align-to position]
  (let [bytes-over (mod position align-to)]
    (if (pos? bytes-over)
      (- align-to bytes-over)
      0)))

(defn trim-to-alignment [align-to binary-coll]
  (let [bytes-off (alignment-padding align-to 
                                     (current-index binary-coll))]
    (with-meta (drop bytes-off binary-coll)
               (update (meta binary-coll) ::index (fnil #(+ bytes-off %) 0)))))


(defprotocol Binary
  :extend-via-metadata true
  (alignment* [this])
  (sizeof* [this]))

(extend-type nil
  BinaryCollection
  (flatten [_] nil)
  Binary
  (alignment* [_] 1)
  (sizeof* [_] 0))

(extend-type clojure.lang.Keyword
  BinaryCollection
  (flatten [_] nil)
  Binary
  (alignment* [_] 1)
  (sizeof* [_] 0))

(extend-type Byte
  BinaryCollection
  (flatten [this] (list this))
  Binary
  (alignment* [_] 1)
  (sizeof* [_] 1))

(extend-type clojure.lang.Sequential
  BinaryCollection
  (flatten [this] 
    (let [coll-alignment (alignment* this)
          [bin max-alignment _] (reduce (fn [[accum max-alignment index] elem]
                                          (let [elem-alignment (alignment* elem)
                                                padding-needed (alignment-padding elem-alignment
                                                                                  index)
                                                new-bytes (concat (repeat padding-needed (byte 0)) elem)
                                                new-index (+ index (count new-bytes))]
                                            [(concat accum new-bytes) (max elem-alignment max-alignment) new-index]))
                                           [[] coll-alignment 0]
                                           (map flatten this))]
      (make-binary bin :alignment max-alignment)))
  Binary
  (alignment* [this] (binary-coll-alignment this))
  (sizeof* [this] (count (flatten this))))


(extend-type clojure.lang.IPersistentMap
  BinaryCollection
  (flatten [this]
    (let [coll-alignment (alignment* this)
          ordered-vals (map #(get this %) (binary-coll-key-order this))
          bin (flatten ordered-vals)]
      (make-binary bin :align (max coll-alignment (binary-coll-alignment bin)))))
  Binary
  (alignment* [this] (binary-coll-alignment this))
  (sizeof* [this] (count (flatten this))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining Codec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Codec
  :extend-via-metadata true
  (encode* [this data])
  (decode* [this binary-seq]))

(defn codec? [x]
  "returns x when x is a codec, nil otherwise"
  (when (or (satisfies? Codec x) (-> x meta (contains? `encode*)))
    x))

(defn alignment [bin]
  (alignment* (specize bin)))

(defn sizeof [bin]
  (sizeof* (specize bin)))

(defn encode [codec data]
  (encode* (specize codec) data))

(defn decode [codec binary-seq]
  (let [c (specize codec)]
    (decode* c (trim-to-alignment (alignment* c) binary-seq))))

(defn codify [x enc dec & {:keys [alignment fixed-size dynamic-size]
                           :or {alignment nil fixed-size nil dynamic-size (constantly nil)}}]
  (with-meta (cond
               (s/spec? x) x
               (ident? x) (specize x)
               :else (s/spec x))
             (assoc (meta x)
                    `encode* enc
                    `decode* dec
                    `alignment* (constantly alignment)
                    `sizeof* (if (some? fixed-size)
                               (constantly fixed-size)
                               dynamic-size))))

(defn specify 
  "Given a list of codecs and specs, specify will create a specified
  codec that will have the same codec properties, and a spec as per
  using s/and on each of the passed specs"
  [& specs-and-codecs]
  (let [the-codec (first (filter codec? specs-and-codecs))]
    (with-meta (s/spec* `(s/and ~@specs-and-codecs))
               (meta the-codec))))

(defn align [codec align-to]
  (codify codec
          (fn [_ data] (align-binary align-to (encode codec data)))
          (fn [_ bin] (decode codec (trim-to-alignment align-to bin)))
          :alignment align-to
          :fixed-size (sizeof codec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Defining Primitive Codecs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-order ByteOrder/LITTLE_ENDIAN)
(def order-map {:little ByteOrder/LITTLE_ENDIAN
                :big ByteOrder/BIG_ENDIAN
                :native (ByteOrder/nativeOrder)
                :network ByteOrder/BIG_ENDIAN})
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

(defn put-prim [put-buffer size order align-to]
  (fn [_ data]
    (let [buff (.order (ByteBuffer/allocate size)
                       (get order-map order default-order))
          _ (put-buffer buff (or data 0))]
      (make-binary (seq (.array buff)) :alignment align-to))))

(defn get-prim [get-buffer size order align-to]
  (fn [_ binary-seq]
    (let [[prim remaining] (split-binary size binary-seq)
          bytes (.order (ByteBuffer/wrap (byte-array prim))
                        (get order-map order default-order))]
      [(get-buffer bytes) remaining])))

(defmacro primitive [prim & {:keys [word-size order]
                             :or {word-size 1}}]
  (let [size `(. ~prim ~'BYTES)
        alignment `(min ~size ~word-size)
        min-value `(. ~prim ~'MIN_VALUE)
        max-value `(. ~prim ~'MAX_VALUE)
        ops `(get primitive-ops ~prim)]
    `(codify (s/spec* (signed-primitive-spec ~min-value ~max-value))
             (put-prim (:put-buffer ~ops) ~size ~order ~alignment)
             (get-prim (:get-buffer ~ops) ~size ~order ~alignment)
             :alignment ~alignment
             :fixed-size ~size)))

(defmacro unsigned-primitive [prim & {:keys [word-size order]
                             :or {word-size 1}}]
  (let [size `(. ~prim ~'BYTES)
        alignment `(min ~size ~word-size)
        max-value `(bit-shift-left 1 (. ~prim ~'SIZE))
        to-unsigned `#(. ~prim toUnsignedLong %)
        ops `(get primitive-ops ~prim)]
    `(codify (s/int-in 0 ~max-value)
             (put-prim (:put-buffer ~ops) ~size ~order ~alignment)
             (get-prim (comp ~to-unsigned (:get-buffer ~ops)) ~size ~order ~alignment)
             :alignment ~alignment
             :fixed-size ~size)))

(defmacro floating-primitive [prim & {:keys [word-size order]
                             :or {word-size 1}}]
  (let [size `(. ~prim ~'BYTES)
        alignment `(min ~size ~word-size)
        min-value `(. ~prim ~'MIN_VALUE)
        max-value `(. ~prim ~'MAX_VALUE)
        ops `(get primitive-ops ~prim)]
    `(codify (s/spec* (floating-primitive-spec ~min-value ~max-value))
             (put-prim (:put-buffer ~ops) ~size ~order ~alignment)
             (get-prim (:get-buffer ~ops) ~size ~order ~alignment)
             :alignment ~alignment
             :fixed-size ~size)))

(s/def ::int8 (primitive Byte))
(s/def ::int16 (primitive Short))
(s/def ::int32 (primitive Integer))
(s/def ::int64 (primitive Long))

(s/def ::uint8 (unsigned-primitive Byte))
(s/def ::uint16 (unsigned-primitive Short))
(s/def ::uint32 (unsigned-primitive Integer))

(s/def ::float (floating-primitive Float))
(s/def ::double (floating-primitive Double))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composite Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sequence-encoder [codec-seq data]
  (map encode codec-seq data))

(defn sequence-decoder [codec-seq bin pred-map]
  (let [{:keys [bytes count until-value while-bytes]
         :or {bytes nil count nil 
              until-value (constantly false) while-bytes (constantly true)}} pred-map
        [bin-to-decode remaining-forced] (if (some? bytes)
                                           (split-binary bytes bin)
                                           [bin nil])
        cs (if (some? count)
             (take count codec-seq)
             codec-seq)
        [data remaining] (reduce (fn [[accum rem-so-far] c]
                                   (if (or (empty? rem-so-far) (not (while-bytes rem-so-far)))
                                     (reduced [accum rem-so-far])
                                     (let [[new-data new-rem] (decode c rem-so-far)
                                           result [(conj accum new-data) new-rem]]
                                       (if (until-value new-data)
                                         (reduced result)
                                         result))))
                                 [[] bin-to-decode]
                                 cs)
        ;; The remaining data will be either from the forced split, or from 
        remaining-final (or remaining-forced remaining)]
    [data remaining-final]))

(defn array
  [codec & {:keys [align kind count max-count min-count distinct gen-max gen]}]
  (let [array-alignment (alignment codec)
        spec (s/spec* `(s/coll-of ~codec
                                  :into []
                                  :kind ~kind
                                  :count ~count
                                  :max-count ~max-count
                                  :min-count ~min-count
                                  :distinct ~distinct
                                  :gen-max ~gen-max
                                  :gen ~gen))]
    (if (some? count)
      (codify spec
              (fn [_ data] (sequence-encoder (repeat count codec) data))
              (fn [_ bin] (sequence-decoder (repeat count codec) bin {}))
              :alignment array-alignment
              :fixed-size (if-let [elem-size (sizeof codec)]
                            (* elem-size count)
                            nil))
      (codify spec
              (fn [_ data] (sequence-encoder (repeat codec) data))
              (fn [this bin] (sequence-decoder (repeat codec) bin (meta this)))
              :alignment array-alignment
              :dynamic-size (if-let [elem-size (sizeof codec)]
                              (fn [this] (if-let [dynamic-count (:count (meta this))]
                                           (* elem-size dynamic-count)
                                   nil))
                              (constantly nil)))
      )))


(defn tuple [])
(defn struct [])
(defn union [])

(defn multicodec [])
(defn cat [])
(defn alt [])
(defn * [])
(defn + [])
(defn ? [])
(defn & [])
(defn nest [])
