(ns sandbox.test.letmap
  (:use [sandbox.letmap] :reload)
  (:use [clojure.test]))

(deftest let-map-test 
  (let [ rect {:w 3, :h 5} 
         test1 (let-map {:a 3, :b 2} (+ a b))
         test2 (let-map rect (* w h))
         test3 (let-map nil (* 1 4)) 
         test4 (let-map {} (* 1 4)) ]
    (is (= test1 5) "Test inline")
    (is (= test2 15) "Test reference")
    (is (= test3 4) "Test nil map")
    (is (= test4 4) "Test empty map")))
