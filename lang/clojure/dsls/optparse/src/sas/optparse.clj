(ns sas.optparse)


"usage: <yourscript> [options] arg1 arg2

options:
  -h, --help            show this help message and exit
  -v, --verbose         make lots of noise [default]
  -q, --quiet           be vewwy quiet (I'm hunting wabbits)
  -f FILE, --filename=FILE
                        write output to FILE
  -m MODE, --mode=MODE  interaction mode: novice, intermediate, or
                        expert [default: intermediate]"
"
usage: <yourscript> [options] arg1 arg2
options:
  -h, --help            show this help message and exit
  -v, --verbose         make lots of noise [default]
  -q, --quiet           be vewwy quiet (I'm hunting wabbits)
  -f FILE, --filename=FILE
                        write output to FILE
  -m MODE, --mode=MODE  interaction mode: novice, intermediate, or
                        expert [default: intermediate]"


(defn build-opt
  [opt]
  (let [flags (take-while string? opt)
        params (drop-while string? opt)
        flags (map (fn [s] (re-pattern (str "^" s ".*"))) flags) ]
    {:flags flags}))

(defmacro optparser
  [ & config ]
  (let [opts (map build-opt config)]
    `(println '~opts)))

;(macroexpand (quote (
(optparser
  [ "-f", "--file" ]  [ "-b", "--bile" ])
;)))
  