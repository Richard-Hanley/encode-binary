(ns encode-binary.core
  (:import java.nio.ByteBuffer)
  (:import java.nio.charset.Charset)
  (:import java.nio.charset.StandardCharsets)
  (:import java.nio.ByteOrder)
  (:require [clojure.spec-alpha2 :as s]
            [clojure.set :as set]))

(defn- meta-merge [obj metadata]
  (with-meta obj
             (merge (meta obj) metadata)))
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

(defn specize [x]
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
  (let [[new-bin rem] (split-at size bin)]
    [new-bin (with-meta rem
                    (update (meta bin) ::index (fnil + 0) size))]))

(defprotocol BinaryCollection
  (flatten [this]))

(defn make-binary 
  [bin & {:keys [key-order alignment padding]}]
  (let [replace-arg #(or %2 %1)]
    (with-meta bin 
               (-> (meta bin)
                   (update ::key-order replace-arg key-order)
                   (update ::alignment replace-arg alignment)
                   (update ::padding replace-arg padding)))))

(defn binary-coll-alignment [bin]
  (or (get (meta bin) ::alignment) 1))

(defn binary-coll-key-order [bin]
  (or (get (meta bin) ::key-order) (keys bin)))

(defn binary-coll-padding [bin]
  (get (meta bin) ::padding))

(defn alignment-padding [align-to position]
  (let [bytes-over (mod position align-to)]
    (if (pos? bytes-over)
      (- align-to bytes-over)
      0)))

(defn trim-to-alignment [align-to binary-coll]
  (let [bytes-off (alignment-padding align-to 
                                     (current-index binary-coll))]
    (with-meta (drop bytes-off binary-coll)
               (update (meta binary-coll) ::index (fnil #(+ bytes-off %) bytes-off)))))

(defn add-padding [pad-to coll]
  (if (some? pad-to)
    (take pad-to (concat coll (repeat (byte 0))))
    coll))

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
      (make-binary (add-padding (binary-coll-padding this) bin) :alignment max-alignment)))
  Binary
  (alignment* [this] (binary-coll-alignment this))
  (sizeof* [this] (count (flatten this))))


(extend-type clojure.lang.IPersistentMap
  BinaryCollection
  (flatten [this]
    (let [coll-alignment (alignment* this)
          ordered-vals (map #(get this %) (binary-coll-key-order this))
          bin (flatten ordered-vals)]
      (make-binary (add-padding (binary-coll-padding this) bin) :align (max coll-alignment (binary-coll-alignment bin)))))
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

(defn alignment 
  ([bin] (alignment* (specize bin)))
  ([bin metadata] (alignment* (meta-merge (specize bin) metadata))))

(defn sizeof 
  ([bin] (sizeof* (specize bin)))
  ([bin metadata] (sizeof* (meta-merge (specize bin) metadata))))


(defn encode 
  ([codec data] (encode* (specize codec) data))
  ([codec data metadata] (encode* (meta-merge (specize codec) metadata) data)))

(defn decode 
  ([codec binary-seq]
   (let [c (specize codec)]
     (decode* c (trim-to-alignment (alignment* c) binary-seq))))
  ([codec binary-seq metadata]
   (let [c (specize codec)]
     (decode* (meta-merge c metadata)
              (trim-to-alignment (alignment* (meta-merge c metadata)) binary-seq)))))

(defn binify [x size-fn align-fn]
  (with-meta x
             (assoc (meta x)
                    `sizeof* size-fn
                    `alignment* align-fn)))

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
  (let [codec (first (filter codec? (map specize specs-and-codecs)))
        specs (map s/form specs-and-codecs)]
    (with-meta (s/spec* `(s/and ~@specs))
               (meta codec))))


(defn align [codec align-to]
  (codify codec
          (fn [_ data] (make-binary (encode codec data) :alignment align-to))
          (fn [_ bin] (decode codec (trim-to-alignment align-to bin)))
          :alignment align-to
          :fixed-size (sizeof codec)))

(defn pad [codec pad-to]
  (codify codec
          (fn [_ data] (make-binary (encode codec data) :padding pad-to))
          (fn [_ bin] (let [[bin-to-decode rem] (split-binary pad-to)
                            [data _] (decode codec bin-to-decode)]
                        [data rem]))
          :alignment (alignment codec)
          :fixed-size pad-to))

(defn static-codec [codec & key-pred-pairs]
  (let [pred-map (apply array-map key-pred-pairs)]
    (codify codec
            (fn [_ data] (encode codec data))
            (fn [_ bin] (decode codec bin pred-map))
            :alignment (alignment codec )
           :fixed-size (sizeof codec pred-map))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common specs that can be used in codec definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/defop constant-field [field value]
  (s/conformer #(assoc % field value)))

(s/defop dependent-field [field f]
  (s/conformer #(assoc % field (f %))))


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

(defn sequence-decoder [codec-seq bin pred-map data-applicators init decoder-deps]
  (let [{:encode-binary.core/keys [bytes count until-value while-bytes]
         :or {bytes nil count nil 
              until-value (constantly false) while-bytes (constantly true)}} pred-map
        [bin-to-decode remaining-forced] (if (some? bytes)
                                           (split-binary bytes bin)
                                           [bin nil])
        cs (if (some? count)
             (take count codec-seq)
             codec-seq)
        [data remaining] (reduce (fn [[accum rem-so-far] [c applicator dep]]
                                   (if (or (empty? rem-so-far) (not (while-bytes rem-so-far)))
                                     (reduced [accum rem-so-far])
                                     (let [[new-data new-rem] (decode c rem-so-far (dep accum pred-map))
                                           result [(conj accum (applicator new-data)) new-rem]]
                                       (if (until-value new-data)
                                         (reduced result)
                                         result))))
                                 [init bin-to-decode]
                                 (map vector cs data-applicators decoder-deps))
        ;; The remaining data will be either from the forced split, or from 
        remaining-final (or remaining-forced remaining)]
    [data remaining-final]))

(defn array
  [codec & {:keys [align kind count max-count min-count distinct gen-max gen]}]
  (let [array-alignment (alignment codec)
        codec-form (s/form codec)
        spec (s/spec* `(s/coll-of ~codec-form
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
              (fn [_ bin] (sequence-decoder (repeat count codec) bin {} (repeat identity) [] (repeat (constantly nil))))
              :alignment array-alignment
              :fixed-size (if-let [elem-size (sizeof codec)]
                            (* (+ elem-size (alignment-padding array-alignment elem-size)) count)
                            nil))
      (codify spec
              (fn [_ data] (sequence-encoder (repeat codec) data))
              (fn [this bin] (sequence-decoder (repeat codec) bin (meta this) (repeat identity) [] (repeat (constantly nil))))
              :alignment array-alignment
              :dynamic-size (if-let [elem-size (sizeof codec)]
                              (fn [this] (if-let [dynamic-count (::count (meta this))]
                                           (* (+ elem-size (alignment-padding array-alignment elem-size)) dynamic-count)
                                   nil))
                              (constantly nil)))
      )))

(defn process-deps 
  "A dependency list is a tuple of keys and maps that have a :spec
  and a :decoder key. This function will take a list of fields keys,
  a dependency list, and produce a tuple of all valid specs and a
  sequence of decoder functions"
  [keys deps]
  (let [dep-map (into {} deps)
        specs (remove nil? (map :spec (vals dep-map)))
        decoder-deps (map #(or (get-in dep-map [% :decoder]) (constantly nil))
                          keys)]
    [(map s/form specs)
     decoder-deps]))

(defn tuple 
  [& {:keys [fields deps]}]
  (let [[spec-deps decoder-deps] (process-deps (range (count fields)) deps)
        symbolic-specs (map (fn [c] (s/form c)) fields)
        spec-form (if (some? spec-deps)
                    `(s/and (s/tuple ~@symbolic-specs)
                            ~@spec-deps)
                    `(s/and (s/tuple ~@symbolic-specs)))]
    (codify (s/spec* spec-form)
            (fn [_ data] (sequence-encoder fields data))
            (fn [_ bin] (sequence-decoder fields bin {} (repeat identity) [] decoder-deps))
            :alignment (apply max (map alignment fields))
            :fixed-size (reduce (fn [a [size align-to]] 
                                  (if (nil? size)
                                    (reduced nil)
                                    (+ a size (alignment-padding align-to a))))
                                0
                                (map #(vector (sizeof %) (alignment %)) fields)))))
  
(defn- convert-field [field]
  (if (keyword? field)
    {:key field :res field}
    field))

(defn unqualified [field]
  (if (keyword? field)
    {:key field :res (keyword (name field)) :un true}
    (assoc :res (keyword (name (:key field))))))

(defn struct 
  [& {:keys [fields deps]}]
  (let [resolved-fields (map convert-field fields)
        req-un (map :key (filter :un resolved-fields))
        req (map :key (filter (complement :un) resolved-fields))
        codecs (map :key resolved-fields)
        result-keys (map :res resolved-fields)
        [spec-deps decoder-deps] (process-deps result-keys deps)
        applicators (map #(fn [data] [% data]) result-keys)
        spec-form (if (some? spec-deps)
                    `(s/and (s/keys :req [~@req] :req-un [~@req-un])
                            ~@spec-deps)
                    `(s/keys :req [~@req] :req-un [~@req-un])) ]
    (codify (s/spec* spec-form)
            (fn [_ data] (sequence-encoder codecs (map #(get data %) result-keys)))
            (fn [_ bin] (sequence-decoder codecs bin {} applicators {} decoder-deps))
            :alignment (apply max (map alignment codecs))
            :fixed-size (reduce (fn [a [size align-to]]
                                  (if (nil? size)
                                    (reduced nil)
                                    (+ a size (alignment-padding align-to a))))
                                0
                                (map #(vector (sizeof %) (alignment %)) codecs)))))
  

;;TODO Figure out padding of a union type
(defn union 
  "Creates a union of types with an optional decoder tag.

  A decoder tag is a function that takes a binary sequence and returns a keyword
  that is one of the field names.

  A field is a fully qualified keyword that has been registered
  while a named field is a list of key-codec pairs."
  [& {:keys [decoder-tag fields named-fields]}]
  ;; Registered fields are their own keys
  (let [name-form-pairs (apply concat (map (fn [[n c]] [n (s/form c)]) (partition 2 named-fields)))
        all-specs (concat (interleave fields fields) name-form-pairs)
        all-fields (concat (interleave fields fields) named-fields)
        field-map (apply array-map all-fields)
        decoder (if (some? decoder-tag)
                  ;;If there is a decoder tag, use it to figure out the type
                  ;;Otherwise look in the metadata
                  (fn [_ bin] (decode (get field-map (decoder-tag bin)) bin))
                  (fn [this bin] (decode (get field-map (::decoder-tag (meta this))) bin)))]
    (codify (s/spec* `(s/or ~@all-specs))
            (fn [_ [tag data]] (encode (get field-map tag) data))
            decoder
            :alignment (reduce max (map alignment (vals field-map)))
            :fixed-size (reduce (fn [a v] (if (nil? v)
                                            (reduced nil)
                                            (max a v)))
                                0
                                (map sizeof (vals field-map))))))

(defn get-mm-sizeof-fn [mm]
  (fn [_]
    (reduce (fn [a v]
              (if-let [size (sizeof (v nil))]
                (max a size)
                (reduced nil)))
            0
            (vals (methods mm)))))

(defn get-mm-alignment-fn [mm]
  (fn [_]
    (reduce (fn [a v] (max a (alignment (v nil))))
            0
            (vals (methods mm)))))

(defn get-mm-enc-fn
  [mm enc-tag]
  (if (some? enc-tag)
   (fn [_ data]
     (encode ((get-method mm (enc-tag data)) nil) data))
   (fn [_ data]
     (encode (mm data) data))))

(defn get-mm-dec-fn
  [mm dec-tag]
  (if (some? dec-tag)
    (fn [_ bin]
      (decode ((get-method mm (dec-tag bin)) nil) bin))
    (fn [this bin]
      (decode ((get-method mm (::decoder-tag (meta this))) nil) bin))))


(defmacro multi-codec 
  [mm retag & {:keys [encoder-tag decoder-tag]}] 
  `(binify (codify (s/multi-spec ~mm ~retag) (get-mm-enc-fn ~mm ~encoder-tag) (get-mm-dec-fn ~mm ~decoder-tag))
           (get-mm-sizeof-fn ~mm)
           (get-mm-alignment-fn ~mm)))

(defmacro symbolic-method 
  [multifn k v c]
  (let [dispatch-fn `(. ~(with-meta multifn {:tag 'clojure.lang.MultiFn}) ~'dispatchFn)
        the-codec `(specify (constant-field :encode-binary.core-test/bType ~v) ~c)]
    `(do
       (let [keyed-spec# (constant-field ~dispatch-fn ~v)
             the-codec# (specify keyed-spec# ~c)]
         (println "Oh wow here - " ~the-codec)
         (defmethod ~multifn ~k [ignore#] ~the-codec))
         ; (defmethod ~multifn ~k [ignore#] the-codec#))
       (defmethod ~multifn ~v [ignore#] ~c))))

  ; (. ~(with-meta multifn {:tag 'clojure.lang.MultiFn}) addMethod ~v (fn [ignore#] ~c))
; (. ~(with-meta multifn {:tag 'clojure.lang.MultiFn}) addMethod ~k  keyed-dispatch#))))

  ; `(do



(defn cat [])
(defn alt [])
(defn * [])
(defn + [])
(defn ? [])
(defn & [])
(defn nest [])
