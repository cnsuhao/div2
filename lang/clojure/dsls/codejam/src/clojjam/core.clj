(ns clojjam.core
  (:use [clojure.string :only (split-lines)]))

; all programs contain T cases first, so technically we can drop the first line



; load data, first 
(defn load-data
  "load the actual data file
    returns [n lines]
   n: number of cases
   lines: remaining lines in the file"
  [path]
  (let [lines (split-lines (slurp path))]
    [(read-string (first lines)) (rest lines)]))



#_(->> lines
  (consume n int lines))
  

#_(codejam-case
  :input
  [(n|num)
   () {n}
   ])

  :num (read-string %)
  
:output
  
  :solution
  ( )
    

  )

(load-data "data/2008-qual-a.txt")