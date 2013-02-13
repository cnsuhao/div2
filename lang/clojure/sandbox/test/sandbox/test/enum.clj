(ns sandbox.test.enum
  (:use [sandbox.enum] :reload)
  (:use [clojure.test])
  (:use [clojure.contrib.with-ns]))

;; test parsing of enum arguments
(deftest bitops.enum.parsing
  (let [parse-enum-arg (with-ns 'sandbox.enum parse-enum-arg)
        parse-enum-args (with-ns 'sandbox.enum parse-enum-args)]
    ; single argument
    (is (= (parse-enum-arg nil {}) {}) "Empty type")
    (is (= (parse-enum-arg Integer {}) {}) "Unknown type")
    
    (is (= (parse-enum-arg "name" {}) {:prefix "name"}) "Prefix as string")
    (is (= (parse-enum-arg 'NAME {}) {:prefix "NAME"}) "Prefix as symbol")
    
    (is (= (parse-enum-arg 'bitmap {}) {:type 'bitmap}) "bitmap type")
    (is (= (parse-enum-arg 0 {}) {:type 0}) "0 based indexing type")
    
    (is (= (parse-enum-arg [] {}) {:attr []}) "vector of attributes")
    (is (= (parse-enum-arg '() {}) {:attr '()}) "list of attributes")
    
    ; looping
    (let [test1 {:attr [], :type 'bitmap}
          test2 {:attr '[A 2 B C], :prefix "K_" ,:type 0}]
      (is (= (parse-enum-args '() {}) {}) "Empty")
      (is (= (parse-enum-args '([] bitmap) {}) test1) "test 1")
      (is (= (parse-enum-args '[K_ 0 (A 2 B C)] {}) test2) "test 2"))))

;; test of grouping by pairs
(deftest bitops.enum.pairs
  (let [pairs(with-ns 'sandbox.enum pairs)]
    (is (= (pairs []) []) "Empty")
    (is (= (pairs '[A]) '[A]) "Single")
    (is (= (pairs '[A B]) '[[A B]]) "Double")
    (is (= (pairs '[A B C D]) '[[A B] [B C] [C D]]) "Many")))

;; testing building all the symbols into a vector
(deftest bitops.enum.build-attrs
  (let [build-attrs (with-ns 'sandbox.enum build-attrs)]
    (is (= (:attr (build-attrs {:type 0, :attr '(A)})) '[[A 0]]) "default (0-index)")
    (is (= (:attr (build-attrs {:type 0, :attr '(A 5)})) '[[A 5]]) "overriding index")
    (is (= (:attr (build-attrs {:type 1, :attr '(A B C)})) '[[A 1][B 2][C 3]]) "1-index")
    (is (= (:attr (build-attrs {:type 0, :attr '(A 1 B C D 1 E)})) '[[A 1][B 2][C 3][D 1][E 2]]) "multiple overriding indices")
    (is (= (:attr (build-attrs {:type 'bitmap, :attr '(A B C D)})) '[[A 1][B 2][C 4][D 8]]) "bitmap indexing")))

(deftest bitops.enum.defenum
;  (with-temp-ns
;    (use 'sandbox.enum)
;    (use 'clojure.test)
    
    (defenum [A B])
    (is (and (= A 0) (= B 1)) (format "default: A=%d B=%d" A B))
    
    (defenum 0 [A B C])
    (is (and (= A 0) (= B 1) (= C 2)) (format "0 indexed: A=%d B=%d C=%d" A B C))
    
    (defenum 1 [A B])
    (is (and (= A 1) (= B 2)) (format "1 indexed: A=%d B=%d" A B))
    
    (defenum K_ [ESC 27])
    (is (= K_ESC 27) (format "prefix: K_ESC=%d" K_ESC))
    
    (defenum [A 1 B C X 0 Y])
    (is (and 
          (= A 1)
          (= B 2)
          (= C 3)
          (= X 0)
          (= Y 1)) (format "multiple overrides: A=%d B=%d C=%d X=%d Y=%d" A B C X Y))
    
    (defenum bitmap [A B C])
    (is (and (= A 1) (= B 2) (= C 4) (format "bitmap indexed: A=%d B=%d C=%d" A B C)))

    (defenum X_ bitmap [A B C])
    (is (and (= X_A 1) (= X_B 2) (= X_C 4) (= X_MASK 7)) (format "bitmap mask: X_A=%d X_B=%d X_C=%d MASK=%d" X_A X_B X_C X_MASK))

    (defenum X_ bitmap [A B C 1])
    (is (and (= X_A 1) (= X_B 2) (= X_C 1) (= X_MASK 3)) (format "bitmap mask w/overrides: X_A=%d X_B=%d X_C=%d MASK=%d" X_A X_B X_C X_MASK)))
