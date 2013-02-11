(ns sandbox.words
  (:use [clojure.string :only [split-lines]]))

                              
"A program to analyze trends in english words"

; English wordlist (DOS format)
; 
; A list of 109582 English words compiled and corrected in 1991 from lists 
; obtained from the Interociter bulletin board. The original read.me file 
; said that the list came from Public Brand Software.
; 
; http://www.sil.org/linguistics/wordlists/english/

(defn i-before-e
  "print some stats on this stupid rule"
  [words]
  (let [valid (filter (fn [s] (re-find #"[^c]ie|cei" s)) words)
        invalid (filter (fn [s] (re-find #"[^c]ei|cie" s)) words)
        n-valid (count valid)
        n-invalid (count invalid)]
    (println "'i' before 'e' (except after 'c')")
    (println "=================================")
    (println (format "# of words which follow rule:  %d" n-valid))
    (println (format "# of words which break rule:  %d" n-invalid))
    (println (format "%% of words which follow rule:  %f" 
                     (* 100 (/ (float n-valid) (+ n-valid n-invalid)))))
    (println))

(defn -main 
  [& args]
  (let [words (split-lines (slurp "wordsEn.txt"))]
    (println (format "Total # of words: %d" (count words)))
    (println)
    (i-before-e words)))
