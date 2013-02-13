(ns sas.config
  (:use [clojure.contrib.str-utils :only (re-split re-sub)])
  (:use [clojure.contrib.duck-streams :only (read-lines)]))


(def #^{:private true} *default-options* {
  :comments "#;"
  :escape "\\\\"
  :re-newline #"(?:\r?\n)|(?:\r\n)"
  :re-setters #"\s*[:=]\s*"
  :re-sect #"^\[\s*((?:\w|[ :])+?)\s*\]\s*$"
  :re-sect-split #"\s+:\s+"
})


(defprotocol ConfigParserProtocol
  "All functionality for a configparser"
  (read-file [x f] "read in a bunch of files")
  (emit-config [x] [x o] "Emit a new config parser passed data")
)

(defn line-group
  [l]
  (and (> (.length l) 0) (= (.substring l 0 1) " ")))

(defn make-keyword
  [k]
  (if (keyword? k) k (keyword (.. k trim toLowerCase))))

(defn parse-config-file
  [lines obj sect]
  ; quit if no more lines
  (if (empty? lines) obj
    (let [ln (first lines)]
      (cond
        ; loop if we have nothing new to work with
        (= (.trim ln) "") (recur (rest lines) obj sect)
        
        ; setup next section there is one
        (not (nil? (re-find (-> obj :options :re-sect) ln)))
        (let [ next-sect 
              (->> (re-find (-> obj :options :re-sect) ln)
                (second)
                (re-split (-> obj :options :re-sect-split))
                (cons :section)
                (map make-keyword)
                (vec))]
          (recur (rest lines) obj next-sect))
        
        ; every other outcome
        :else
          (cond
            ; bash inline document blocking
            (re-find #"\s*(<<.*)\s*$" (first lines))
            (let [eof (str (second (re-find #"\s*(<<.*)\s*$" (first lines))))
                  re (re-pattern (format "^%s\\s*" eof))
                  [lns rst] (split-with #(nil? (re-find re %1)) (rest lines))
                  m (map #(.trim %1) lns) ]
                (println (apply str (interpose "\n" m)))
                (recur (rest rst) obj sect)))

            ; other wise standard line adding
            
            :else (recur (rest lines) obj sect)
            
            
            ))))

;split-with
            ; handles giant bodies
              ;(doseq [l (take-while #(not (= ky (.trim %2))) (rest lines))]
              ;  (println l))
              
               ;    ll (->>  (rest lines)
               ;        (take-while #(not (= ky (.trim %))))
               ;        (#(do (doseq [i %] (println ky i)) %1))
               ;        (map #(if (%1) 
               ;        (map #(.trim %))
               ;        (interpose " ")
               ;        (apply str))]
              ;(println ll)
              ;(recur (drop-while #(not (= ky (.trim %))) (rest lines)) obj sect)
            
            
            ; check for expanded bodies

              
              
       ;       [[a b] (re-split (-> obj :options :re-setters) ln 2)
       ;       [k v] [(conj sect (make-keyword a)) (if (string? b) (.trim b) b) ]]
       ;       #_(->> (take 5 (rest lines))
       ;         (#(do (println %) %))
       ;         (map #(.trim %))
       ;         (interpose \ )
       ;         (apply str v)
       ;         (#(do (println %) %)))
       ;   (recur (rest lines) obj sect))
        
        


      
      
;      )))
;      (cond
;        
;        (= (when (string? ln) (.trim ln)) "") (recur (rest lines) obj sect)
;        
;        ; check for a new section 
;        (not (nil? (re-find (-> obj :optsions :re-sect) ln)))
;           (do (println lines obj sect))
;        ;(let [next-sect
              ;(->> (re-find (-> obj :optsions :re-sect) ln)
       ;         (#(do (println %) %))
        ;        (re-find (-> obj :options :re-sect) ln)
         ;       (#(do (println %) %))
          ;      (second)
           ;     (re-split (-> obj :options :re-sect-split))
            ;    (cons :sections)
             ;   (map make-keyword)
             ;   (vec))])));
          

(defrecord ConfigParser [options defaults schema global sections]
  ConfigParserProtocol
  (read-file [this f]
    (->> (read-lines f)                                    ; split lines
      (map #(re-sub (-> this :options :re-comments) "" %)) ; remove all comments
      (drop-while #(= % ""))                               ; clear extra lines
      (#(parse-config-file % this [nil]))
      (#(do (println (:sections %1)) %1))
      (emit-config this)))
  
  (emit-config [this] (emit-config this this))
  (emit-config [this obj]
    (let [{opts :options, dflts :defaults, schma :schema, glbl :global sects :sections} obj]
      (ConfigParser. opts dflts schma glbl sects))))

(defn parse-statement
  [[kw & args]]
  (let [ strkw (str (. kw sym))
         typ (if (not (= \s (last strkw))) (keyword (str strkw \s)) kw) ]
    (cond 
      (= kw :schema) nil
      (not (= \s (last strkw))) (cons typ (list (vec args)))
      :else (cons typ args))))

(defn fix-keys
  [[typ & args]]
  (->> args
    (map (fn [[k v]] [(if (keyword? k) [k] k) v]))
    (map (fn [[k v]] [(map keyword k) v]))
    (map (fn [[k v]] [(cons typ k) v]))))

(defn parse-lines
  [lines]
  (let [statements (map parse-statement lines)
        pairs (mapcat fix-keys statements) ]
    (loop [pairs pairs obj {:options *default-options* :schema nil :sections nil :defaults nil} ]
      (if (empty? pairs) obj
        (let [ [k v] (first pairs) ]
          (recur (rest pairs) (assoc-in obj k v)))))))

(defn create-comments-regexp
  [obj]
  (re-pattern (format "(?<!%s)[%s].*$" (-> obj :options :escape) (-> obj :options :comments))))

(defn setup-config-parser
  [& lines]
  (-> (parse-lines lines)
    (#(assoc-in %1 [:options :re-comments] (create-comments-regexp %1)))))


(defmacro config-parser
  [& lines]
  `(let [ obj# (setup-config-parser ~@lines)
           {opts# :options, dflts# :defaults, schma# :schema, glbl# :global, sects# :sections} obj#]
       (ConfigParser. opts# dflts# schma# glbl# sects#)))


;#_(config-parser
   ;(:option  [other kk] "#;")
   ;(:option  :comments "#;")
   ;(:default [:email :from] "someone@example.com")
   ;(:defaults ([email from] "someone@example.com")
      ;        ([:email :to] "someone@example.com")
     ;         (:all "me")))
   
   ;#_(:schema
     ;(f (-get [v] ...)
     ;   (-set [v] ...))
     
     ;([:email :body] -get [^String v] ...)
     ;([:email :body] -get [v] ...))

(use 'clojure.contrib.trace)
(dotrace  [] (read-file (config-parser) "sample.cfg"))
;(def cfg (read-file (config-parser) "sample.cfg"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;