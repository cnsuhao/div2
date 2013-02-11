(ns sandbox.test.macros
  (:use [sandbox.macros] :reload)
  (:use [clojure.test]))

;; test the math macro
(deftest math-macro
  (is (math 1) 1)
  (is (math 1 + 3) 4)
  (is (math 1 + 3 - 2) 2)
  (is (math 1 * 5) 5)
  (is (math 1 * 5 -2) 5))