(ns sas.test.bitops
  (:use [sas.bitops] :reload)
  (:use [clojure.test]))

;; test to make sure bitmaps work
(deftest bitmap-test 
  (let [[a b c] [1 2 4]]
    (is (= (bitmap) 0) "Empty bitmap equals 0")
    (is (= (bitmap 1) 1) "Identity")
    (is (= (bitmap 7) 7) "Complicated value (non 2^n)")
    (is (= (bitmap 1 2 8) 11) "Multiple values")
    (is (= (bitmap 1 1 3 4) 7) "Repeated values")
    (is (= (bitmap a b c) 7) "Variables")
    (is (= (apply bitmap '[1 3]) 3) "(apply bitmap args...)")))

(deftest nbits-test
  (let [ qq (range 1 21)
         aa [ 1 2 2 3 3 3 3 4 4 4
             4 4 4 4 4 5 5 5 5 5] 
         test (fn [q a] (is (= (nbits q) a) (format "nbits: q=%d a=%d" q a)))]
    (map test qq aa)))

(deftest bitmask-test
  (let [ qq (range 1 9)
         aa [ 1 3 7 15 31 63 127 255 ] 
         test (fn [q a] (is (= (bitmask q) a) (format "bitmask: q=%d a=%d" q a)))]
    (map test qq aa)))

(deftest bitmask-seq-test
  (is (= (bitmask-seq 0x04040404 8) '(4 4 4 4)) "simple parse")
  (is (= (bitmask-seq 0x07060504 8) '(4 5 6 7)) "order is reversed"))
(bitmask-seq 0x3344 8)