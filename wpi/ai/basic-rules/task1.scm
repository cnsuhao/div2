; Main Functions
(include "./lib/data.scm")
(include "./lib/debug.scm")
(include "./lib/grid.scm")
(include "./lib/rules.scm")


; add to stress according to increase stress rule
(defrule 'increase-the-stress (priority 8) :
  (''increase-stress exists)
  =>
  (let ((stress (fact-value 'stress))
        (val    (fact-value 'increase-stress)))
    (printf "Stress increased...~a~n" (+ stress val))
    (assert 'stress is (+ stress val))
    (retract 'increase-stress)))


; The end condition, called when stress is high enough
(defrule 'check-my-stress (priority 5):
  (''stress is > 10)
  =>
  (printf "that's it!  I can't take anymore!~n~n")
  (stop))

; The end condition, called when stress is high enough
(defrule 'check-my-relaxation (priority 5):
  (''stress is < 0)
  =>
  (printf "Well, it didn't kill me.  And no more stress! ~n~n")
  (stop))

; a rule to trigger a relax
(defrule 'attempt-relax (priority 0) :
  (''relax is 0)
  =>
  (printf "Ahh...Relaxation~n")
  (assert 'relax is (fact-value 'relaxtime))
  (assert 'increase-stress is -4))

; a rule to depricate relaxing (only after tasks)
(defrule 'relax-counter :
  (not (''relax is 0))
  =>
  (let ((val (fact-value 'relax)))
    (printf "Relax Counter now ~a~n" (- val 1))
    (assert 'relax is (- val 1))))


;; A series of rules to check for tasks, harder tasks take a high priority
; find hard tasks and do them
(defrule 'find-hard-task (priority 3):
  (bind ('?task ('?task exists)))
  ('?task is ''hard)
  =>
  (printf "Increase stress by 3~n")
  (assert 'increase-stress is 3)
  (retract ?task))

; find fair task (medium)
(defrule 'find-fair-task (priority 2):
  (bind ('?task ('?task exists)))
  ('?task is ''fair)
  =>
  (printf "Increase stress by 2~n")
  (assert 'increase-stress is 2)
  (retract ?task))

; find easy task
(defrule 'find-tast-task (priority 1):
  (bind ('?task ('?task exists)))
  ('?task is ''easy)
  =>
  (printf "Increase stress by 1~n")
  (assert 'increase-stress is 1)
  (retract ?task))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   FACTS   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffacts
  ('ai-project1 is 'hard)
  ('tech1-project2 is 'hard)
  ('jazz-comp2 is 'easy)
  ('ai-exam is 'fair)
  ('tech1-exam is 'easy)
  
  ('stress is 0)
  ('relax is 0)
  
  ('relaxtime is 5))

(debugmode #f)