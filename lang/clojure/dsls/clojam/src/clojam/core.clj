(ns clojam.core)

(ns clojam.core
  (:use [clojure.string :only (join split split-lines trim)]))

; all programs contain T cases first, so technically we can drop the first line

(def parsers
  {:num read-string 
   :nums (fn [line] (map read-string (split line #" +")))})

; load data, first 
(defn load-data
  "load the actual data file
    returns [n lines]
   n: number of cases
   lines: remaining lines in the file"
  [path]
  (let [lines (split-lines (trim (slurp path)))]
    [(read-string (first lines)) (rest lines)]))

(defmacro bind-map
  [hmap & body]
  (let [bindings (mapcat (fn [k] [(.sym k) (hmap k)]) (keys hmap))]
  `(let [~@bindings]
     ~@body)))


(defn parse-stmt-multiline
  [[cmap lines parsers] k-name k-parser k-count]
  (let [n (cmap k-count)
        values (take n lines)
        values (if k-parser (map (parsers k-parser) values) values)]
    (println "multi grabbed" k-name n values)
    [(assoc cmap k-name values) (drop n lines) parsers]))


(defn parse-stmt-line
  [[cmap lines parsers] k-name k-parser]
  (let [value (first lines)
        value (if ~k-parser ((parsers ~k-parser) value) value)]
    (println "line grabbed" k-name value)
     [(assoc cmap k-name value) (rest lines) parsers]))


(defn parse-stmt-tester
  [& args]
  (println args)
  (first args))


(defn parse-stmts
  [stmts]
  (loop [results [] stmts stmts ]
    (let [stmt (first stmts)]
      (cond (nil? stmt) results   ; return case map when no more statements
            
          ; if this is a symbol and the next item is a vector
            (and (symbol? stmt) (vector? (second stmts)))
            (let [s-name stmt 
                  s-count (first (second stmts))
                  parts (map keyword (split (str s-name) #"\|"))
                  k-name (first parts)
                  k-parser (second parts)
                  k-count (keyword (str s-count))]
            
              (recur (conj results (list parse-stmt-tester k-name k-parser k-count)) (drop 2 stmts)))
      
          ; if statement is a symbol
            (symbol? stmt)
            (let [s-name stmt
                  parts (map keyword (split (str s-name) #"\|"))
                  k-name (first parts)
                  k-parser (second parts)]
              (recur (conj results (list parse-stmt-tester k-name k-parser)) (rest stmts)))
            

            :else stmt))))


(defmacro run-test
  [stmts & lines]
  (println (map type (map first (parse-stmts stmts))))
  `(let [data#
         (-> [{} ~lines parsers]
             ~@(parse-stmts stmts))]
     ))

;  `(reduce 
;     (fn [args# f#] (f# args#))
;     [{} ~lines parsers]
;     ~(parse-stmts stmts)))

(macroexpand '(run-test (S|num letters [S]) "3" "a" "b" "c" "4" "x" "y" "z" "w"))

(run-test (S|num letters [S]) "3" "a" "b" "c" "4" "x" "y" "z" "w")
            
(defmacro defquestion
  [name & references]
  (let [docstring (when (string? (first references)) (first references))
        references (if docstring (next references) references)
        name (if docstring (vary-meta name assoc :doc docstring) name)
        
        metadata (when (map? (first references)) (first references))
        references (if metadata (next references) references)
        name (if docstring (vary-meta name merge metadata) name)
        
        output (rest (first (filter #(= :output (first %)) references)))
        references (remove #(= :output (first %)) references)
        
        input (rest (first (filter #(= :input (first %)) references)))
        references (remove #(= :input (first %)) references)
        
        body (rest (first (filter #(= :case (first %)) references)))
        references (remove #(= :case (first %)) references)
        
        parsers (merge parsers (apply hash-map (apply concat references)))]
    `(let [output# (partial apply ~@output)
           parsers# ~parsers
           case-parser# ~(parse-stmts input)]
       (defn ~name
         [path#]
         (let 
           [[n-cases# lines#] (load-data path#)
            cases# 
            (loop [results# [] data# lines#]
             (if 
               (empty? data#) results#
               (let [case# (reduce (fn [args# f#] (f# args#)) [{} data# parsers#] case-parser#)]
                 (recur (conj results# (first case#)) (second case#)))))]
           (println cases#))))))

(macroexpand '(defquestion numbers
  (:output (format "%d"))
  (:input S|num
          engines [S]
          Q|num2
          queries [Q])
  (:num2 read-string)
  (:case 
    (println S Q)
    [S Q])))

(defquestion numbers
  (:output (format "%d"))
  (:input S|num
          engines [S]
          Q|num2
          queries [Q])
  (:num2 read-string)
  (:case 
    (println S Q)
    [S Q]))

(numbers "data/2008-r1a-c.txt")