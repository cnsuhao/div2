(ns gcjam.parser
  (:use [clojure.contrib.string :only (replace-char split split-lines trim)]))

(def *rootpath* (format "data/%s" (replace-char \. \/ (str *ns*))))

(defn readlines
  [pth]
  (split-lines (slurp pth)))

;; conversion functions
(def conv {
   'double 'Double/parseDouble
   'int 'Integer/parseInt
})

(defmacro with-spec 
  [ typed args & body ]
  (let [ args (if args args ())
         sym (map last (partition 2 typed)) ]
    `(fn [~@sym ~@args] 
       (let [~@(mapcat (fn [[f s]] [s (list (conv f) s)]) (partition 2 typed))] ~@body))))

(defn parse-line
  [f l & args]
  (apply f (concat (split #" " (trim l)) args)))

(defmacro defparser
  [ parser-name & specs ]
  (let [ specs (mapcat (fn [[n t a & b]] [ n (macroexpand `(with-spec ~t ~a ~@b)) ]) specs) ]
	  `(defn ~parser-name
	     [pth#]
	     (let [~@specs]
	       (let [ lines# (readlines pth#) ]
	         (parse-line ~'init (first lines#) (next lines#)))))))


(def get-path #(format "%s/%s.%s" %1 %2 %3))

(defprotocol SimpleProcessor
  "Basic interface for handling Google Code Jam problems"
  (init [self] "loads path")
  (parse-in [self] "loads in data and builds output to be processed")
  (run-test [self] "run on each test")
  (output [self] "formats the output"))
  