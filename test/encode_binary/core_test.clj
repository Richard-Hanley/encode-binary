(ns encode-binary.core-test
    (:require [clojure.test :refer :all]
              [clojure.spec-alpha2 :as s]
              [encode-binary.core :as e]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Redefining primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def :little/int8 (e/primitive Byte))
(s/def :little/int16 (e/primitive Short))
(s/def :little/int32 (e/primitive Integer))
(s/def :little/int64 (e/primitive Long))
(s/def :little/uint8 (e/unsigned-primitive Byte))
(s/def :little/uint16 (e/unsigned-primitive Short))
(s/def :little/uint32 (e/unsigned-primitive Integer))
(s/def :little/float (e/floating-primitive Float))
(s/def :little/double (e/floating-primitive Double))

(s/def :big/int8 (e/primitive Byte ::e/order :big))
(s/def :big/int16 (e/primitive Short ::e/order :big))
(s/def :big/int32 (e/primitive Integer ::e/order :big))
(s/def :big/int64 (e/primitive Long ::e/order :big))
(s/def :big/uint8 (e/unsigned-primitive Byte ::e/order :big))
(s/def :big/uint16 (e/unsigned-primitive Short ::e/order :big))
(s/def :big/uint32 (e/unsigned-primitive Integer ::e/order :big))
(s/def :big/float (e/floating-primitive Float ::e/order :big))
(s/def :big/double (e/floating-primitive Double ::e/order :big))

(s/def :aligned/int8 (e/primitive Byte ::e/word-size 4))
(s/def :aligned/int16 (e/primitive Short ::e/word-size 4))
(s/def :aligned/int32 (e/primitive Integer ::e/word-size 4))
(s/def :aligned/int64 (e/primitive Long ::e/word-size 4))
(s/def :aligned/uint8 (e/unsigned-primitive Byte ::e/word-size 4))
(s/def :aligned/uint16 (e/unsigned-primitive Short ::e/word-size 4))
(s/def :aligned/uint32 (e/unsigned-primitive Integer ::e/word-size 4))
(s/def :aligned/float (e/floating-primitive Float ::e/word-size 4))
(s/def :aligned/double (e/floating-primitive Double ::e/word-size 4))

(s/def :aligned-wide/int8 (e/primitive Byte ::e/word-size 8))
(s/def :aligned-wide/int16 (e/primitive Short ::e/word-size 8))
(s/def :aligned-wide/int32 (e/primitive Integer ::e/word-size 8))
(s/def :aligned-wide/int64 (e/primitive Long ::e/word-size 8))
(s/def :aligned-wide/uint8 (e/unsigned-primitive Byte ::e/word-size 8))
(s/def :aligned-wide/uint16 (e/unsigned-primitive Short ::e/word-size 8))
(s/def :aligned-wide/uint32 (e/unsigned-primitive Integer ::e/word-size 8))
(s/def :aligned-wide/float (e/floating-primitive Float ::e/word-size 8))
(s/def :aligned-wide/double (e/floating-primitive Double ::e/word-size 8))



(testing "primitive specs"
  (testing "signed"
    (testing "zero checking"
      (is (= 0 (s/conform :little/int8 0)))
      (is (= 0 (s/conform :little/int16 0)))
      (is (= 0 (s/conform :little/int32 0)))
      (is (= 0 (s/conform :little/int64 0)))
      (is (= 0.0 (s/conform :little/float 0.0)))
      (is (= 0.0 (s/conform :little/double 0.0))))
    (testing "max value checking"
      (testing "at max value"
        (is (= Byte/MAX_VALUE (s/conform :little/int8 Byte/MAX_VALUE)))
        (is (= Short/MAX_VALUE (s/conform :little/int16 Short/MAX_VALUE)))
        (is (= Integer/MAX_VALUE (s/conform :little/int32 Integer/MAX_VALUE)))
        (is (= Long/MAX_VALUE (s/conform :little/int64 Long/MAX_VALUE)))
        (is (= Float/MAX_VALUE (s/conform :little/float Float/MAX_VALUE)))
        (is (= Double/MAX_VALUE (s/conform :little/double Double/MAX_VALUE))))
      (testing "beyond max value"
        (is (s/invalid? (s/conform :little/int8 (inc Byte/MAX_VALUE))))
        (is (s/invalid? (s/conform :little/int16 (inc Short/MAX_VALUE))))
        (is (s/invalid? (s/conform :little/int32 (inc Integer/MAX_VALUE))))))
    (testing "min value checking"
      (testing "at max value"
        (is (= Byte/MIN_VALUE (s/conform :little/int8 Byte/MIN_VALUE)))
        (is (= Short/MIN_VALUE (s/conform :little/int16 Short/MIN_VALUE)))
        (is (= Integer/MIN_VALUE (s/conform :little/int32 Integer/MIN_VALUE)))
        (is (= Long/MIN_VALUE (s/conform :little/int64 Long/MIN_VALUE)))
        (is (= Float/MIN_VALUE (s/conform :little/float Float/MIN_VALUE)))
        (is (= Double/MIN_VALUE (s/conform :little/double Double/MIN_VALUE))))
      (testing "beyond max value"
        (is (s/invalid? (s/conform :little/int8 (dec Byte/MIN_VALUE))))
        (is (s/invalid? (s/conform :little/int16 (dec Short/MIN_VALUE))))
        (is (s/invalid? (s/conform :little/int32 (dec Integer/MIN_VALUE))))))
    (testing "type checking"
      (is (s/invalid? (s/conform :little/int8 1.02)))
      (is (s/invalid? (s/conform :little/int16 1.02)))
      (is (s/invalid? (s/conform :little/int32 1.02)))
      (is (s/invalid? (s/conform :little/int64 1.02)))))
  (testing "unsigned"
    (testing "zero checking"
      (is (= 0 (s/conform :little/uint8 0)))
      (is (= 0 (s/conform :little/uint16 0)))
      (is (= 0 (s/conform :little/uint32 0))))
    (testing "max value checking"
      (testing "at max value"
        (is (= 0xff (s/conform :little/uint8 0xff)))
        (is (= 0xffff (s/conform :little/uint16 0xffff)))
        (is (= 0xffffffff (s/conform :little/uint32 0xffffffff))))
      (testing "beyond max value"
        (is (s/invalid? (s/conform :little/uint8 0x100)))
        (is (s/invalid? (s/conform :little/uint16 0x10000)))
        (is (s/invalid? (s/conform :little/uint32 0x100000000)))))
    (testing "negative values"
      (is (s/invalid? (s/conform :little/uint8 -1)))
      (is (s/invalid? (s/conform :little/uint16 -1)))
      (is (s/invalid? (s/conform :little/uint32 -1))))
    (testing "type checking"
      (is (s/invalid? (s/conform :little/int8 1.02)))
      (is (s/invalid? (s/conform :little/int16 1.02)))
      (is (s/invalid? (s/conform :little/int32 1.02)))
      (is (s/invalid? (s/conform :little/int64 1.02))))))


