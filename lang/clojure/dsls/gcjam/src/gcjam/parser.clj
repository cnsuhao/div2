(ns gcjam.parser
  (:use [clojure.contrib.string :only (split)]
        [gcjam.helpers]))

;;;;;;;;;;;;;
;; GLOBALS ;;
;;;;;;;;;;;;;

(derive java.util.regex.Pattern ::regex)

;;;;;;;;;;;;;;;;;;;;;;
;;  PARSER METHODS  ;;
;;;;;;;;;;;;;;;;;;;;;;

  #_(call [this prsr s _]
    (let [ ans (re-find re s)
           wrd (if (seq? ans) (first ans) ans)
           rm  (.trim (substring s (.length wrd))) ]
      [(func ans) rm]))
  

;;;;;;;;;;;;;;;;;;;;
;;  BUILD PARSER  ;;
;;;;;;;;;;;;;;;;;;;;

(defn build-inst-test
  [inst]
  (class (first inst)))

(defmulti build-inst build-inst-test)
(defmethod build-inst ::regex [inst] "regex")
(defmethod build-inst :default [inst] "default")

(defn build-statement
  [obj stmt]
  (if (seq? stmt)
    (assoc obj (keyword (first stmt)) (build-inst (drop 2 stmt)))
    obj))
    ;(merge obj (. (eval stmt) fns))))

(defmacro build-parser
  [& stmts]
  `(let [prsr# ~(reduce build-statement {} stmts) ]
     prsr#))

(build-parser
  (int => #"\d+" (fn [n] (Integer/parseInt n)))
  (default => #"\w+" identity ))