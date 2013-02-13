(ns sas.gcjam.parser)

 
(derive java.util.regex.Pattern ::regex)


(defmulti build-inst #^{:private true}
  "Builds an individual instruction by figuring out which method type to use"
  (fn [inst] (class (first inst))))

; build a regex method
(defmethod build-inst ::regex 
  [inst] 
  (let [re (first inst)
        func (eval (second inst))]
  (_RegexParserMethod. re func)))

; stardard method
(defmethod build-inst :default [inst] 
  (let [[cmds frmt] (inst-parts inst)
        cmds (map split-cmd (flatten cmds))]
  (_StandardParserMethod. cmds frmt)))


(defn- build-statement
  [obj stmt]
  ; if stmt is a list...
  (if (seq? stmt) 
    ; associate the first keyword with the rest of the instruction, skipping the "=>"
    (assoc obj (keyword (first stmt)) (build-inst (drop 2 stmt)))
    ; else attempt to merge
    (merge obj (. (eval stmt) fns))))

(defn- build-parser*
  "Function which actually creates the new parser"
  [& stmts]
  #_(_Parser. (reduce build-statement {} stmts) (atom nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CONVINIENCE MACROS  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro build-parser
  "builds a new parser"
  [& stmts]
  (apply build-parser* stmts))

(defmacro defparser
  "Defines a new parser"
  [nm & stmts]
  `(def ~nm ~(apply build-parser* stmts)))

(def a 3)
