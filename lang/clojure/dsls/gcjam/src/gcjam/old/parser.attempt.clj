(ns gcjam.parser
  (:use [clojure.contrib.string :only (split)]
        [gcjam.helpers]))

;(defgrammar q2008b
;  (file => num-cases cases|num-cases)  
;  (num-cases => (v.int) :as v)
;  (cases => (num-lines.int)
;            (NA.int NB.int)
;            timetables|num-lines)
;  (timetables => (dep.datetime arr.datetime))
;  (datetime   => "(\d):(\d)([ap]m?)" | int.hour int.min :as [hour min]))

;(defgrammar q2008b
;  (file => num-cases cases|num-cases)  
;  (num-cases => (v.int) :as v)
;  (cases => (num-lines.int)
;            (NA.int NB.int)
;            timetables|num-lines)
;  (timetables => (dep.datetime arr.datetime))
;  (datetime   => "(\d):(\d)([ap]m?)" | int.hour int.min :as [ hour min ]))

; int "\d+"
; file => unknown since it just calls the others...
; num-cases => (v.int) => (\d+)\n(.*)
; cases => (\d+)\n

;;;;;;;;;;;;;
;; GLOBALS ;;
;;;;;;;;;;;;;
(derive java.util.regex.Pattern ::regex)
;(derive clojure.lang.Keyword ::keyword)

;;;;;;;;;;;;;;;;;;
;; TEMP GLOBALS ;;
;;;;;;;;;;;;;;;;;;
(def d (slurp "data/qr2010a/A-small-practice.in"))
(def *base-grammar* {
  :int (fn [prsr s] (Integer/parseInt s))
})

;;;;;;;;;;;;;;;
;;  HELPERS  ;;
;;;;;;;;;;;;;;;

(defn statement-parts
  [coll]
  (let [[a b] (split-with #(not (isa? :as %)) coll)]
    [ a (when (not-empty? b) (rest b)) ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  INSTRUCTION BUILDERS  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol AbstractParser
  "parser abstraction"
  (get [x] [x y]))

(defmulti build-inst (fn [stmt _] (class (first stmt))))

;; a pure method build, takes all parameters but does not require further parsing
(defmethod build-inst ::regex [stmt prsr]
  (let [ re (first stmt)
         stmt (rest (rest stmt))            ; rest rest to skip "|" symbol
         [dfn frmt] (statement-parts stmt)]
  (println "regex build")
  (println "-------------")
  (println re)
  (println dfn)
  (println frmt)
  (println "")
  (fn [prsr s]  
))

(defmethod build-inst :default [stmt _] "partial build")

#_(defn build-inst
  [stmts]
  (let [dfn  (take-while #(not (isa? % :as)) stmts)           ; definition/how to parse
        frmt (rest (drop-while #(not (isa? % :as)) stmts))    ; return value
        cmds (filter symbol? dfn)
        rslt `rslt#]
    (println cmds)
    (println frmt)
  `(fn [prsr# s#]     
     (let [~rslt (-> {} )]
        ~(if (empty? frmt) rslt "format here")))))

;; pseudo code of any given parser command
;;   applies a series of commands to a new object, then returns the result after a potential transformation
;;   
;;   splits each bit into a section...ignore newlines for now
;;   takes some of the string, returns next bit all ready
;;           it'd help to premptively break up string?
;;   takes in the string 

(defn process-statement
  [stmt] 
  (if (.startsWith (str stmt) "(")
    `(assoc '~(first stmt) ~(build-inst (rest (rest stmt))))    ; rest rest to skip the '=>
    `(merge (~stmt))))

(defmacro process-statements
  [& stmts]
  (let [inst (map process-statement stmts)]  
    `(-> *base-grammar* ~@inst)))

;; creates and defines a parser based on other parsers
(defmacro defparser
  [nm & stmts]
  `(let [prsr# (process-statements ~@stmts)]
     (def ~nm
       ([] prsr#)
       ([k#] (prsr# k#)))))

#_(defparser a 
  (file => num-cases cases|num-cases :as 3)
)

;;;;;;;;;;;
;; DEBUG ;;
;;;;;;;;;;;

(def s "3:45p")
(def dt '(#"(\d):(\d)" | h.int m.int :as [ (+ h p) m ]))   ;([ap]m?)
(def tst (build-inst dt {}))


;;; parsers are multimethods which take multiple variable types
;;;    :rules is used internally and it is for extension
;;;    pass it a string and it runs the primary method ('parse')
;;;    pass it a symbol and it returns the method

#_(defparser *base-grammar*
    (int => #"\d+" | v :as (Integer/parseInt v))
)

#_(
;; each statement starts with a split of a certain type
;; normal splits are ' ' and '\n' though any can exist
;; nesting results in nested maps

(defn parse-statement
  [s]
  (let [ rslt
    (-> [{} s]  ; loop through with 1 per statement     
      
      )]
    rslt))      ; as statement

(defn consume-line
  [[obj s] f]
  (let [parts (split "#\n" 2 s)]
    [ (f [obj (first parts)]) (rest parts) ]))

(defn num-cases
  [[obj s]]
  (let [rslt 
    (-> {}
      (consume-line (fn [[obj s]] (assoc obj :v (Integer/parseInt s)))))]
    rslt))

(def fns
  (-> {}
    (assoc :num-cases (fn [x] x))))

)