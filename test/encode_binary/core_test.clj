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

(s/def :big/int8 (e/primitive Byte :order :big))
(s/def :big/int16 (e/primitive Short :order :big))
(s/def :big/int32 (e/primitive Integer :order :big))
(s/def :big/int64 (e/primitive Long :order :big))
(s/def :big/uint8 (e/unsigned-primitive Byte :order :big))
(s/def :big/uint16 (e/unsigned-primitive Short :order :big))
(s/def :big/uint32 (e/unsigned-primitive Integer :order :big))
(s/def :big/float (e/floating-primitive Float :order :big))
(s/def :big/double (e/floating-primitive Double :order :big))

(s/def :aligned/int8 (e/primitive Byte :word-size 4))
(s/def :aligned/int16 (e/primitive Short :word-size 4))
(s/def :aligned/int32 (e/primitive Integer :word-size 4))
(s/def :aligned/int64 (e/primitive Long :word-size 4))
(s/def :aligned/uint8 (e/unsigned-primitive Byte :word-size 4))
(s/def :aligned/uint16 (e/unsigned-primitive Short :word-size 4))
(s/def :aligned/uint32 (e/unsigned-primitive Integer :word-size 4))
(s/def :aligned/float (e/floating-primitive Float :word-size 4))
(s/def :aligned/double (e/floating-primitive Double :word-size 4))

(s/def :aligned-wide/int8 (e/primitive Byte :word-size 8))
(s/def :aligned-wide/int16 (e/primitive Short :word-size 8))
(s/def :aligned-wide/int32 (e/primitive Integer :word-size 8))
(s/def :aligned-wide/int64 (e/primitive Long :word-size 8))
(s/def :aligned-wide/uint8 (e/unsigned-primitive Byte :word-size 8))
(s/def :aligned-wide/uint16 (e/unsigned-primitive Short :word-size 8))
(s/def :aligned-wide/uint32 (e/unsigned-primitive Integer :word-size 8))
(s/def :aligned-wide/float (e/floating-primitive Float :word-size 8))
(s/def :aligned-wide/double (e/floating-primitive Double :word-size 8))



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


(testing "primitive codec binary"
  (let [byte-test {:value 0x17 :little (map unchecked-byte [0x17]) :big (map unchecked-byte [0x17])} 
        short-test {:value 0x1234 :little (map unchecked-byte [0x34 0x12]) :big (map unchecked-byte [0x12 0x34])}
        int-test {:value (unchecked-int 0xDEADBEEF)
                  :little (map unchecked-byte [0xEF 0xBE 0xAD 0xDE])
                  :big (map unchecked-byte [0xDE 0xAD 0xBE 0xEF])} 
        long-test {:value 0x12345678DEADBEEF 
                   :little (map unchecked-byte [0xEF 0xBE 0xAD 0xDE 0x78 0x56 0x34 0x12]) 
                   :big (map unchecked-byte [0x12 0x34 0x56 0x78 0xDE 0xAD 0xBE 0xEF])}]
    (testing "encoding"
      (testing "nil value encoding"
        (is (= (repeat 1 0) (e/encode :little/int8 nil)))
        (is (= (repeat 2 0) (e/encode :little/int16 nil)))
        (is (= (repeat 4 0) (e/encode :little/int32 nil)))
        (is (= (repeat 8 0) (e/encode :little/int64 nil))))
      (testing "little endian"
        (is (= (:little byte-test) (e/encode :little/int8 (:value byte-test))))
        (is (= (:little short-test) (e/encode :little/int16 (:value short-test))))
        (is (= (:little int-test) (e/encode :little/int32 (:value int-test))))
        (is (= (:little long-test) (e/encode :little/int64 (:value long-test)))))
      (testing "big endian"
        (is (= (:big byte-test) (e/encode :big/int8 (:value byte-test))))
        (is (= (:big short-test) (e/encode :big/int16 (:value short-test))))
        (is (= (:big int-test) (e/encode :big/int32 (:value int-test))))
        (is (= (:big long-test) (e/encode :big/int64 (:value long-test))))))
    (testing "decoding"
      (testing "little endian"
        (is (= (:value byte-test) (first (e/decode :little/int8 (:little byte-test)))))
        (is (= (:value short-test) (first (e/decode :little/int16 (:little short-test)))))
        (is (= (:value int-test) (first (e/decode :little/int32 (:little int-test)))))
        (is (= (:value long-test) (first (e/decode :little/int64 (:little long-test))))))
      (testing "big endian"
        (is (= (:value byte-test) (first (e/decode :big/int8 (:big byte-test))))
        (is (= (:value short-test) (first (e/decode :big/int16 (:big short-test))))
        (is (= (:value int-test) (first (e/decode :big/int32 (:big int-test))))
        (is (= (:value long-test) (first (e/decode :big/int64 (:big long-test))))))))))
    (testing "unsigned decoding"
      (is (= 0xFE (first (e/decode :little/uint8 [(unchecked-byte 0xFE)]))))
      (is (= 0xDEADBEEF (first (e/decode :little/uint32 (:little int-test))))))))


