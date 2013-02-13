(ns sas.test.ini
  (:use [sas.test.ini] :reload)
  (:use [clojure.test])
  (:use [clojure.contrib.with-ns]))

(deftest clean-lines-test
  (let [ clean-line (with-ns sas.ini clean-line) ]
    
))


