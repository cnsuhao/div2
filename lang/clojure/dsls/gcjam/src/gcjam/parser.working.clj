(ns gcjam.parser
  (:use [clojure.contrib.string :only (split)]
        [gcjam.helpers]))

;;;;;;;;;;;;;
;; GLOBALS ;;
;;;;;;;;;;;;;

(derive java.util.regex.Pattern ::regex)

;;;;;;;;;;;;;;;;;
;;  PROTOCOLS  ;;
;;;;;;;;;;;;;;;;;

(defprotocol _AbstractParser
  "Parser protocol"
  (get-string [this])
  (set-string [this s])
  (get-fn [this k])
  (parse [this s]))

(defprotocol _AbstractParserMethod
  "ParserMethod protocol"
  (copy [this])
  (call [this prsr obj] [this prsr s obj]))

;;;;;;;;;;;;;;
;;  PARSER  ;;
;;;;;;;;;;;;;;

(deftype _Parser [fns string]
  _AbstractParser
  (get-string [this] (deref string))
  (set-string [this s] (reset! string s))
  (get-fn [this k] (get fns k (fns :default)))
  (parse [this s] (set-string this s)(call (get-fn this :parse) this nil)))

;;;;;;;;;;;;;;;;;;;;;;
;;  PARSER METHODS  ;;
;;;;;;;;;;;;;;;;;;;;;;

(deftype _RegexParserMethod [re func]
  _AbstractParserMethod
  (call [this prsr _] 
    (let [ s (get-string prsr) 
           [rslt rm] (call this prsr s _) ]
      (set-string prsr rm)
      rslt))
  (call [this prsr s _]
    (let [ ans (re-find re s)
           wrd (if (seq? ans) (first ans) ans)
           rm  (.trim (substring s (.length wrd))) ]
      [(func ans) rm])))

(deftype _StandardParserMethod [cmds frmt]
  _AbstractParserMethod
  (call [this prsr obj] 
    (let [ s (get-string prsr)
           [rslt rm] (call this prsr s obj) ]
      (set-string prsr rm)
      rslt))
  (call [this prsr s obj]
    (loop [cmds cmds rslt {} s s]
      (if (empty? cmds) [(if (nil? frmt) rslt (eval-template `(letmap ~rslt ~@frmt))) s]
        (let [[k f r] (first cmds)
              func (get-fn prsr f)
              rpt (if (keyword? r) (get rslt r (get obj r)) r)
              [ans rm] (if (nil? rpt)
                         (call func prsr s rslt)
                         (loop [c rpt ans [] s s]
                           (if (zero? c) [ans s]
                             (let [[a r] (call func prsr s rslt)]
                               (recur (dec c) (conj ans a) r)))))
              rslt (assoc rslt k ans)]
          (recur (rest cmds) rslt rm))))))

;;;;;;;;;;;;;;;;;;;;
;;  BUILD PARSER  ;;
;;;;;;;;;;;;;;;;;;;;

(defn inst-parts
  [coll]
  (let [[a b] (split-with #(not (isa? :as %)) coll)]
    [ a (when (not-empty? b) (rest b)) ]))

(defn split-cmd
  [sym-cmd]
  (let [[cmd rpt] (split #"\|" (str sym-cmd)) 
        [cmd func] (split #"\." cmd)
        rpt (if (nil? rpt) nil (try (Integer/parseInt rpt) (catch NumberFormatException _ (keyword rpt))))]
    [(keyword cmd) (keyword (or func cmd)) rpt]))

(defmulti build-inst (fn [inst] (class (first inst))))

; build the regex parser
(defmethod build-inst ::regex [inst] 
  (let [re (first inst)
        func (eval (second inst))]
  (_RegexParserMethod. re func)))

(defmethod build-inst :default [inst] 
  (let [[cmds frmt] (inst-parts inst)
        cmds (map split-cmd (flatten cmds))]
  (_StandardParserMethod. cmds frmt)))
  
(defn build-statement
  [obj stmt] 
  (if (seq? stmt)
    (assoc obj (keyword (first stmt)) (build-inst (drop 2 stmt)))
    (merge obj (. (eval stmt) fns))))

(defn build-parser*
  [& stmts]
  (_Parser. (reduce build-statement {} stmts) (atom nil)))

(defmacro build-parser
  [& stmts]
  (apply build-parser* stmts))

(defmacro defparser
  [nm & stmts]
  `(def ~nm ~(apply build-parser* stmts)))

;;;; TESTS
(defparser *base-parser* 
  (int => #"\d+" (fn [n] (Integer/parseInt n)))
  (default => #"\w+" identity ))

(defparser p 
  *base-parser*
  (parse => (linecount.int) lines|linecount)
  (lines => count.int words|count :as (apply str (interpose \  words)) ))
(parse p "3\n3 quick brown fox\n2 red herring\n4 steve is a tool")

(defparser q2008b
  *base-parser*
  (parse => (num-cases.int) cases|num-cases)  
  (cases => (num-lines.int)
            (NA.int NB.int)
            timetables|num-lines)
  (timetables => (dep.time arr.time))
  (time   => #"(?i)(\d+):(\d+)[^ap]?([ap]?)m?" datetime-time))

(def inp (slurp "data/test.in"))
(parse q2008b inp)