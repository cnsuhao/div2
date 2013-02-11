(ns sandbox.test.jokes
  (:use [sandbox.jokes] :reload)
  (:use [clojure.test]))

(deftest cross-joke
  (is (cross "elephant" "rhino") "elephino"))