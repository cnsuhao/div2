
; Main Functions
(include "./lib/data.scm")
(include "./lib/debug.scm")
(include "./lib/grid.scm")
(include "./lib/rules.scm")

; Task two specific files
(include "./ext/gridhelpers.scm")
(include "./ext/generators.scm")
(include "./ext/rulehelpers.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   LANDSCAPE   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; construct a 3x3 grid which tracks 'vehicles and 'elevation
(define final-grid (make-grid
                    ('cell ('veh 'elv))
                    (((#f 0) (#f 0) (#f 0))
                     ((#f 0) (#f 0) (#f 0))
                     ((#f 0) (#f 0) (#f 0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   FACTS   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; rules work by removing options from grid facts
;   a fact looks along the lines of 'cellXxY.type is (list)
;        eg:   cell0x0.veh is '(a b c d e f g h i)
;
;   rules remove options from facts until an answer can be inserted into the grid
(generate-facts final-grid '(a b c d e f g h i) (list 1 2 3 4 5 6 7 8 9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   RULES   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; RULES BASED UPON THE NOISY NEIGHBORS LOGIC PUZZLE


;;;;;;;;;;
;;;;;;;;;;    Completetion Rules
;;;;;;;;;;        Rules to check if an answer has been found
;;;;;;;;;;        

; rule to determine solved grid
(defrule 'done (priority 10) :
  (bind ('?c ('?c cell-contains ''done)))
  (''is grid-filled ''final-grid)
  =>
  (printf "~n------------~nI'm done!~n")
  (stop))

; checks for cells with single values
(defrule 'single (priority 5) :
 (bind ('?cell ('?cell exists)))
 ('?cell num-values 1)
  =>
  (let* (( val (car (fact-value ?cell)))
         ( pos (get-coords (symbol->string ?cell)))
         ( gc  (grid-ref final-grid (posn.x pos) (posn.y pos))))
    (printf "~a found as single value..Removing everywhere else~n" val)
    (remove-from-all val)
    (assert ?cell is (list val 'done))
    (printf "Placing at ~a,~a on landscape~n" (posn.x pos) (posn.y pos))
    (if (symbol? val)
        (grid-set! final-grid (posn.x pos) (posn.y pos) (make-cell val (cell.elv gc)))
        (grid-set! final-grid (posn.x pos) (posn.y pos) (make-cell (cell.veh gc) val)))
    (print-facts facts)))





;;;;;;;;;;
;;;;;;;;;;    Constraint Rules
;;;;;;;;;;        These rules eliminate options from stored facts
;;;;;;;;;;        


;;;;  The Person in Vehicle B sees an elevation of 8 to the north

; b south of 8
(defrule 'b-southof-8 :
  (bind ('?cell ('?cell cell-contains ''b)))
  (not ('?cell south-of 8))
  =>
  (remove-from-fact ?cell 'b)
  (printf "~a is not B~n" ?cell))

; 8 north of b
(defrule '8-northof-b :
  (bind ('?cell ('?cell cell-contains 8)))
  (not ('?cell north-of ''b))
  =>
  (remove-from-fact ?cell 8)
  (printf "~a is not 8~n" ?cell))



;;;; The Person in Vehicle C sees an elevation of 7 to the south, 6 on either the east or west, and only has two people next to him

; 2 neighbors
(defrule 'c-numneighbors-2 :
  (bind ('?cell ('?cell cell-contains ''c)))
  (not ('?cell num-neighbors 2))
  =>
  (remove-from-fact ?cell 'c)
  (printf "~a is not C~n" ?cell))


; c north of 7
(defrule 'c-northof-7 :
  (bind ('?cell ('?cell cell-contains ''c)))
  (not ('?cell north-of 7))
  =>
  (remove-from-fact ?cell 'c)
  (printf "~a is not C~n" ?cell))

; 7 south of c
(defrule '7-southof-c :
  (bind ('?cell ('?cell cell-contains 7)))
  (not ('?cell south-of ''c))
  =>
  (remove-from-fact ?cell 7)
  (printf "~a is not 7~n" ?cell))


; c east/west of 6
(defrule 'c-east/westof-6 :
  (bind ('?cell ('?cell cell-contains ''c)))
  (not (('?cell east-of 6) or ('?cell west-of 6)))
  =>
  (remove-from-fact ?cell 'c)
  (printf "~a is not C~n" ?cell))

; 6 east/west of c
(defrule '6-east/westof-c :
  (bind ('?cell ('?cell cell-contains 6)))
  (not (('?cell east-of ''c) or ('?cell west-of ''c)))
  =>
  (remove-from-fact ?cell 6)
  (printf "~a is not 6~n" ?cell))



;;;; The Person in Vehicle D is sandwiched between 2 and 5 on his east/west side and south of A

; D between 2 and 5
(defrule 'd-between-2-and-5 :
  (bind ('?cell ('?cell cell-contains ''d)))
  (not ((('?cell east-of 2) and ('?cell west-of 5)) or
        (('?cell east-of 5) and ('?cell west-of 2))))
  =>
  (remove-from-fact ?cell 'd)
  (printf "~a is not D~n" ?cell))

; 5 east/west of d 
(defrule '5-east/westof-d :
  (bind ('?cell ('?cell cell-contains 5)))
  (not (('?cell east-of ''d) or ('?cell west-of ''d)))
  =>
  (remove-from-fact ?cell 5)
  (printf "~a is not 5~n" ?cell))

; 2 east/west of d
(defrule '2-east/westof-d :
  (bind ('?cell ('?cell cell-contains 2)))
  (not (('?cell east-of ''d) or ('?cell west-of ''d)))
  =>
  (remove-from-fact ?cell 2)
  (printf "~a is not 2~n" ?cell))


; d south of a
(defrule 'd-southof-a :
  (bind ('?cell ('?cell cell-contains ''d)))
  (not ('?cell south-of ''a))
  =>
  (remove-from-fact ?cell 'd)
  (printf "~a is not D~n" ?cell))

; a north of d
(defrule 'a-northof-d :
  (bind ('?cell ('?cell cell-contains ''a)))
  (not ('?cell north-of ''d))
  =>
  (remove-from-fact ?cell 'a)
  (printf "~a is not A~n" ?cell))


;;;; The Person in Vehicle E is east/west of a and south of 9

; e south of 9
(defrule 'e-southof-9 :
  (bind ('?cell ('?cell cell-contains ''e)))
  (not ('?cell south-of 9))
  =>
  (remove-from-fact ?cell 'e)
  (printf "~a is not E~n" ?cell))

; 9 north of e
(defrule '9-northof-b :
  (bind ('?cell ('?cell cell-contains 9)))
  (not ('?cell north-of ''e))
  =>
  (remove-from-fact ?cell 9)
  (printf "~a is not 9~n" ?cell))


; e east/west of a
(defrule 'e-east/westof-a :
  (bind ('?cell ('?cell cell-contains ''e)))
  (not (('?cell east-of ''a) or ('?cell west-of ''a)))
  =>
  (remove-from-fact ?cell 'e)
  (printf "~a is not E~n" ?cell))

; a east/west of e
(defrule 'a-east/westof-e :
  (bind ('?cell ('?cell cell-contains ''a)))
  (not (('?cell east-of ''e) or ('?cell west-of ''e)))
  =>
  (remove-from-fact ?cell 'a)
  (printf "~a is not A~n" ?cell))


;;;; The Person in Vehicle F is south of E and is standing at 5

; f is on 5
(defrule 'f-on-5 :
  (bind ('?cell ('?cell is-on ''f)))
  (not ('?cell cell-contains 5))
  =>
  (remove-from-fact ?cell 'f)
  (printf "~a is not F~n" ?cell))

; 5 is on f
(defrule 'f-on-5 :
  (bind ('?cell ('?cell cell-contains 5)))
  (not ('?cell is-on ''f))
  =>
  (remove-from-fact ?cell 5)
  (printf "~a is not 5~n" ?cell))


; f south of e
(defrule 'f-southof-e :
  (bind ('?cell ('?cell cell-contains ''f)))
  (not ('?cell south-of ''e))
  =>
  (remove-from-fact ?cell 'f)
  (printf "~a is not F~n" ?cell))

; e north of f
(defrule 'f-northof-e :
  (bind ('?cell ('?cell cell-contains ''e)))
  (not ('?cell north-of ''f))
  =>
  (remove-from-fact ?cell 'e)
  (printf "~a is not E~n" ?cell))



;;;; The Person in Vehicle G is east/west of 1 and south of 4

; g south of 4
(defrule 'g-southof-4 :
  (bind ('?cell ('?cell cell-contains ''g)))
  (not ('?cell south-of 4))
  =>
  (remove-from-fact ?cell 'g)
  (printf "~a is not G~n" ?cell))

; 4 north of g
(defrule '4-northof-g :
  (bind ('?cell ('?cell cell-contains 4)))
  (not ('?cell north-of ''g))
  =>
  (remove-from-fact ?cell 4)
  (printf "~a is not 4~n" ?cell))


; g east/west of 1
(defrule 'g-east/westof-1 :
  (bind ('?cell ('?cell cell-contains ''g)))
  (not (('?cell east-of 1) or ('?cell west-of 1)))
  =>
  (remove-from-fact ?cell 'g)
  (printf "~a is not G~n" ?cell))

; 1 east/west of g
(defrule '1-east/westof-g :
  (bind ('?cell ('?cell cell-contains 1)))
  (not (('?cell east-of ''g) or ('?cell west-of ''g)))
  =>
  (remove-from-fact ?cell 1)
  (printf "~a is not 1~n" ?cell))


;;;; The Person in Vehicle I is north of 3 and west of 8 and west of h

; i north of 3
(defrule 'i-northof-3 :
  (bind ('?cell ('?cell cell-contains ''i)))
  (not ('?cell north-of 3))
  =>
  (remove-from-fact ?cell 'i)
  (printf "~a is not I~n" ?cell))

; 3 south of i
(defrule '3-southof-i :
  (bind ('?cell ('?cell cell-contains 3)))
  (not ('?cell south-of ''i))
  =>
  (remove-from-fact ?cell 3)
  (printf "~a is not 3~n" ?cell))


; i west of 8
(defrule 'i-westof-8 :
  (bind ('?cell ('?cell cell-contains ''i)))
  (not ('?cell west-of 8))
  =>
  (remove-from-fact ?cell 'i)
  (printf "~a is not I~n" ?cell))

; 8 east of i
(defrule '8-eastof-i :
  (bind ('?cell ('?cell cell-contains 8)))
  (not ('?cell east-of ''i))
  =>
  (remove-from-fact ?cell 8)
  (printf "~a is not 8~n" ?cell))



; i west of h
(defrule 'i-eastof-h :
  (bind ('?cell ('?cell cell-contains ''i)))
  (not ('?cell west-of ''h))
  =>
  (remove-from-fact ?cell 'i)
  (printf "~a is not I~n" ?cell))

; h east of i
(defrule 'h-westof-i :
  (bind ('?cell ('?cell cell-contains ''h)))
  (not ('?cell east-of ''i))
  =>
  (remove-from-fact ?cell 'h)
  (printf "~a is not H~n" ?cell))

;;;; 3 is south of 6 but north of 1

; 3 south of 6 and north of 1
(defrule '3-southof-6-northof-1 :
  (bind ('?cell ('?cell cell-contains 3)))
  (not (('?cell north-of 1) and ('?cell south-of 6)))
  =>
  (remove-from-fact ?cell 3)
  (printf "~a is not 3~n" ?cell))

; 1 south of 3
(defrule '1-southof-3 :
  (bind ('?cell ('?cell cell-contains 1)))
  (not ('?cell south-of 3))
  =>
  (remove-from-fact ?cell 1)
  (printf "~a is not 1~n" ?cell))

; 6 north of 3
(defrule '6-northof-3 :
  (bind ('?cell ('?cell cell-contains 6)))
  (not ('?cell north-of 3))
  =>
  (remove-from-fact ?cell 6)
  (printf "~a is not 6~n" ?cell))