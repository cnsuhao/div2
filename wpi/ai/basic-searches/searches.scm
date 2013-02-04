(require "./data.scm" "data.scm")
(require "./debug.scm" "debug.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    TEST GRIDS    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  TEST GRID 1
;  Provided by project description

;X - - - - 
;- X * * - 
;- * * * - 
;- * * X - 
;- - - - X 
(define test1 (make-grid
 ('cell ('obstacle 'vehicle 'elevation 'toll))
 ( (('pit #f 0 0) (#f   #f 0 0)   (#f #f 0 0)  (#f   #f 0 0)  (#f   #f 0 0))
   ((#f   #f 0 0) ('pit #f 0 10)  (#f #f 4 10) (#f   #f 2 10) (#f   #f 0 0))
   ((#f   #f 0 0) (#f   #f 4 10)  (#f #f 6 10) (#f   #f 4 10) (#f   #f 0 0))
   ((#f   #f 0 0) (#f   #f 2 10)  (#f #f 4 10) ('pit #f 0 10) (#f   #f 0 0))
   ((#f   #f 0 0) (#f   #f 0 0)   (#f #f 0 0)  (#f   #f 0 0)  ('pit #f 0 0)))))

;  TEST GRID 2
;  Cannot be solved

;- X * - - 
;X X * * - 
;* * * * - 
;- * * X - 
;- - - - X 
(define test2 (make-grid
 ('cell ('obstacle 'vehicle 'elevation 'toll))
 ( ((#f #f 0  0) ('p #f 0  0)  (#f #f 8 10) (#f #f 6 0))
   (('p #f 0  0) ('p #f 0  0)  (#f #f 4 10) (#f #f 2 0))
   ((#f #f 8 10) (#f #f 4 10)  (#f #f 6 10) (#f #f 4 0))
   ((#f #f 6  0) (#f #f 2 10)  (#f #f 4 10) (#f #f 0 0)))))


;  TEST GRID 3
;  Worst case for depth first

;- X - - - - - 
;X - - - - - - 
;- - - - X X X 
;- - - X - - - 
;- - - X - - - 
;- - - X - - - 
(define test3 (make-grid
 ('cell ('obstacle 'vehicle 'elevation 'toll))
 (((#f #f 0 0)('p #f 0 0)(#f #f 0 0)(#f #f 0 0)(#f #f 0 0)(#f #f 0 0)(#f #f 0 0))
  (('p #f 0 0)(#f #f 0 0)(#f #f 0 0)(#f #f 0 0)(#f #f 0 0)(#f #f 0 0)(#f #f 0 0))
  ((#f #f 0 0)(#f #f 0 0)(#f #f 0 0)(#f #f 0 0)('p #f 0 0)('p #f 0 0)('p #f 0 0))
  ((#f #f 0 0)(#f #f 0 0)(#f #f 0 0)('p #f 0 0)(#f #f 0 0)(#f #f 0 0)(#f #f 0 0))
  ((#f #f 0 0)(#f #f 0 0)(#f #f 0 0)('p #f 0 0)(#f #f 0 0)(#f #f 0 0)(#f #f 0 0))
  ((#f #f 0 0)(#f #f 0 0)(#f #f 0 0)('p #f 0 0)(#f #f 0 0)(#f #f 0 0)(#f #f 0 0)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    GRID HELPER    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; valid-cell grid int int -> bool
;;  If cell is valid, it returns the cell, otherwise false (on grid and non obstacle/vehicle)
(define (valid-cell g x y)
  (begin 
    (if (or (or (< x 0) (>= x (grid.width g)))
            (or (< y 0) (>= y (grid.height g)))) #f ; out of bounds
                           
      ; check if valid (return cell if it is)
      (let ((cell (grid-ref g x y)))
        (if (not (or (cell.vehicle cell) (cell.obstacle cell))) cell #f)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   PATH HELPERS   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; path-contains path posn -> bool
; returns whether or not path contains a node already
(define (path-contains apath aposn)
  (let loop ((p apath))
    (if (null? p) #f
        (if (posn-eqv? aposn (car p)) #t
            (loop (cdr p))))))

; extend-path grid path dirs -> list of paths
; extends a node of a path in all possible directions (valid-cell)
(define (extend-path grid path dirs)
  ; if there is no path, nothing to expand
  (if (null? path) ()
      ; get the first nod and loop through expandable directions (posns)
      (let ((node (car path)))
        (let loop ((dirs dirs))
          ; if no more to expand, return empty list
          (if (null? dirs) ()
              (let ((cell (posn+ node (car dirs))))
                (if (and (valid-cell grid (posn.x cell) (posn.y cell))
                         (not (path-contains path cell)))
                    (cons (cons cell path) (loop (cdr dirs)))
                    (loop (cdr dirs)))))))))
                    
; converts symbols 'n, 'ne', 'e etc into posn's
(define-syntax directions
  (syntax-rules()
    ((directions dirs)
     (begin
       (filter (lambda(v) (not (eqv? v #f)))
       (map (lambda (d)
              (cond [(eqv? d 'n)  (make-posn  0 -1)]
                    [(eqv? d 'ne) (make-posn  1 -1)]
                    [(eqv? d 'e)  (make-posn  1  0)]
                    [(eqv? d 'se) (make-posn  1  1)]
                    [(eqv? d 's)  (make-posn  0  1)]
                    [(eqv? d 'sw) (make-posn -1  1)]
                    [(eqv? d 'w)  (make-posn -1  0)]
                    [(eqv? d 'nw) (make-posn -1 -1)]
                    [else #f])) dirs))))))                    

; plot-path path grid -> grid (same one)
; set the vehicles path through the grid
(define (plot-path apath agrid)
  (let loop ((path apath))
    (if (null? path) agrid
        (let ((pos (car path)))
          (set!cell.vehicle (grid-ref agrid (posn.x pos) (posn.y pos)) 't)
          (loop (cdr path))))))

; path-cost path grid -> num
; calculate the cost of traversing a given path
(define (path-cost apath agrid)
  (let loop ((path apath))
    ; if first point, then cost is free
    (if (null? (cdr path)) 0
        (let* ((end   (car path))
               (start (cadr path))
               (ecell (grid-ref agrid (posn.x end) (posn.y end)))
               (scell (grid-ref agrid (posn.x start) (posn.y start))))
          (+ (+ (if (> (cell.toll ecell) (cell.toll scell)) (cell.toll ecell) 0) ; toll if new toll is greater than old
                (- 1 (/ (- (cell.elevation scell) (cell.elevation ecell)) 10))) ; gas = gas * (1-dElevation/10)
             (loop (cdr path)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    DEBUG   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; print-search str posn posn list -> void
; displays initial search info
; ie:  Depth-First Search:
;           Start: x,y
;           End  : x,y
;           Directions: N, NE

(define (print-search astr start end dirs)
  ;start on a new line
  (printline #f)
  
  ; print search
  (printfline (string-append astr ":"))
  
  (let ((indent "   "))
    ; print start
    (printfline (string-append indent (string-append "Start: " (posn->str start))))
    ; print end
    (printfline (string-append indent (string-append "End: " (posn->str end))))
    
    ; print directions checked
    (let loop ((dirs dirs)
               (dir-s (string-append indent "Directions: ")))
      (if (null? dirs) (printfline dir-s)
          (let ((new-s (string-append dir-s (string-append (symbol->string (car dirs)) ", "))))
            (loop (cdr dirs) new-s))))))

; print-path list -> str
; displays path from start to end
; ie:  (1,1)->(1,2)->(2,3)...

(define (path->str path)
  ; print final path
  (let ((str 
    ; loop through posns
    (let loop ((path path))
      (if (null? path) ":"
          (string-append (loop (cdr path)) (string-append (posn->str (car path)) "->"))))))
    ; chop off last bit
    (list->string (let loop ((l (string->list str)))
      (if (= (length l) 2) ()
          (cons (car l) (loop (cdr l))))))))



; print-grid grid -> void
; displays the grid's obstacles and toll
; ie:  X - - - -
;      - X * * -
;      - * * * -
;      - * * X -
;      - - - - X
(define (print-grid grid)
  (let ((data (vector->list (grid.data grid))))    ; get all the data

    ; start on a new line
    (printline #f)
    ; loop through rows
    (let loop ((rows data))
      (if (null? rows) (printline #f)  ; if no more rows, return
          (begin
            ; loop through cells
            (let loop ((row (vector->list (car rows)))
                       (s ""))
              (if (null? row) (printfline s) ;hit the end of the row, print
                  (let* ((cell   (car row))  ; get cell
                         (cell-s (cond [(cell.obstacle cell) "X "]        ; calc cell's string
                                       [(cell.vehicle cell) "+ "]
                                       [(> (cell.toll cell) 0) "* "]
                                       [else "- "]))
                         (new-s  (string-append s cell-s)))
                    ; loop
                    (loop (cdr row) new-s))))
            (loop (cdr rows)))))
    
    (printfline "X = Blocked")
    (printfline "* = Toll")
    (printfline "+ = Path\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    SEARCH   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  DEPTH FIRST SEARCH  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (depthsearch grid start end d)
   ;print-search type and info
  (print-search "Depth-First Search" start end d)
  ; display grid
  (print-grid grid)
  
  ; check if points are valid, if they are not quit out
  (if (not (and (valid-cell grid (posn.x start) (posn.y start))
                (valid-cell grid (posn.x end) (posn.y end))))
      (printfline "start or end is invalid")
      
      ;  start and end points are valid, setup paths with root
      (time (let ((paths (list (list start)))
            (dirs (directions d)))

        (printfline "Beginning Search!")
        ; start looping through paths
        (let loop ((paths paths)
                   (np 1) (ns 0)) ; number of paths checked and number stored (total)
          (printfline (string-append "Current Path" (path->str (car paths))))   ; print current (top)
          ; are we at the goal?
          (if (posn-eqv? (car (car paths)) end)
              (begin (printfline "Search Successful!\n\n")
                     (print-grid (plot-path (car paths) grid))
                     (printfline (string-append "Final Path" (path->str (car paths))))
                     (printfline (string-append "Path Cost: " (number->string (path-cost (car paths) grid))))
                     (printfline (string-append "Number of Paths Checked: " (number->string np)))
                     (printfline (string-append "Number of Paths Currently Stored: " (number->string (length paths))))
                     (printfline (string-append "Number of Paths Stored Total: " (number->string ns))))
              
              ;if not, keep looking
              (let* ((curpath  (car paths))  ; remove first node
                     (newpaths (begin (printfline "-Expanding Current Nodes") ; expand (also filters out duplicates
                                      (extend-path grid curpath dirs))))
                ; display new paths
                (if (null? newpaths) (printfline "   No new paths, backtracking\n")
                    (begin (printfline "-Paths Added:")
                       (let loop ((npaths newpaths))
                         (if (null? npaths) (printline #f)
                             (begin (printfline (string-append "   " (path->str (car npaths))))
                                    (loop (cdr npaths)))))))
                ; append to the beginning
                (loop (append newpaths (cdr paths)) (+ 1 np) (+ ns (length newpaths))))))))))
                 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  BREADTH FIRST SEARCH  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (breadthsearch grid start end d)
   ;print-search type and info
  (print-search "Breadth-First Search" start end d)
  ; display grid
  (print-grid grid)
  
  ; check if points are valid, if they are not quit out
  (if (not (and (valid-cell grid (posn.x start) (posn.y start))
                (valid-cell grid (posn.x end) (posn.y end))))
      (printfline "start or end is invalid")
      
      ;  start and end points are valid, setup paths with root
      (time (let ((paths (list (list start)))
            (dirs (directions d)))

        (printfline "Beginning Search!")
        ; start looping through paths
        (let loop ((paths paths)
                   (np 1) (ns 0)) ; number of paths checked and number stored (total)
          (printfline (string-append "Current Path" (path->str (car paths))))   ; print current (top)
          ; are we at the goal?
          (if (posn-eqv? (car (car paths)) end)
              (begin (printfline "Search Successful!\n\n")
                     (print-grid (plot-path (car paths) grid))
                     (printfline (string-append "Final Path" (path->str (car paths))))
                     (printfline (string-append "Path Cost: " (number->string (path-cost (car paths) grid))))
                     (printfline (string-append "Number of Paths Checked: " (number->string np)))
                     (printfline (string-append "Number of Paths Currently Stored: " (number->string (length paths))))
                     (printfline (string-append "Number of Paths Stored Total: " (number->string ns))))
              
              ;if not, keep looking
              (let* ((curpath  (car paths))  ; remove first node
                     (newpaths (begin (printfline "-Expanding Current Nodes") ; expand (also filters out duplicates
                                      (extend-path grid curpath dirs))))
                ; display new paths
                (if (null? newpaths) (printfline "   No new paths.\n")
                    (begin (printfline "-Paths Added:")
                       (let loop ((npaths newpaths))
                         (if (null? npaths) (printline #f)
                             (begin (printfline (string-append "   " (path->str (car npaths))))
                                    (loop (cdr npaths)))))))
                ; append to the beginning
                (loop (append (cdr paths) newpaths) (+ 1 np) (+ ns (length newpaths))))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;
;;  BEST FIRST SEARCH  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(define (bestsearch grid start end d)
   ;print-search type and info
  (print-search "Best-First Search" start end d)
  ; display grid
  (print-grid grid)
  
  ; check if points are valid, if they are not quit out
  (if (not (and (valid-cell grid (posn.x start) (posn.y start))
                (valid-cell grid (posn.x end) (posn.y end))))
      (printfline "start or end is invalid")
      
      ;  start and end points are valid, setup paths with root
      (time (let ((paths (list (list start)))
            (dirs (directions d)))

        (printfline "Beginning Search!")
        ; start looping through paths
        (let loop ((paths paths)
                   (np 1) (ns 0)) ; number of paths checked and number stored (total)
          ; are their any paths?
          (if (null? paths) (printfline "Search Unsuccessful!\n\n") (begin
          
          (printfline (string-append "Current Path" (path->str (car paths))))   ; print current (top)
          ; are we at the goal?
          (if (posn-eqv? (car (car paths)) end)
              (begin (printfline "Search Successful!\n\n")
                     (print-grid (plot-path (car paths) grid))
                     (printfline (string-append "Final Path" (path->str (car paths))))
                     (printfline (string-append "Path Cost: " (number->string (path-cost (car paths) grid))))
                     (printfline (string-append "Number of Paths Checked: " (number->string np)))
                     (printfline (string-append "Number of Paths Currently Stored: " (number->string (length paths))))
                     (printfline (string-append "Number of Paths Stored Total: " (number->string ns))))
                     
              ;if not, keep looking
              (let* ((curpath  (car paths))  ; remove first node
                     (newpaths (begin (printfline "-Expanding Current Nodes") ; expand (also filters out duplicates
                                      (extend-path grid curpath dirs))))
                ; display new paths
                (if (null? newpaths) (printfline "   No new paths.\n")
                    (begin (printfline "-Paths Added:")
                       (let loop ((npaths newpaths))
                         (if (null? npaths) ()
                             (begin (printfline (string-append "   " (path->str (car npaths))))
                                    (loop (cdr npaths)))))))
                ; append to the beginning
                (set! paths (append (cdr paths) newpaths))
                (set! paths (sort paths (lambda (p1 p2) (< (path-cost p1 test1) (path-cost p2 test1)))))
                (printfline (string-append "Cheapest Path" (path->str (car paths))))
                (printfline (string-append "Path Cost: " (number->string (path-cost (car paths) grid))))
                (printline #f)
                (loop (sort paths (lambda (p1 p2) (< (path-cost p1 test1) (path-cost p2 test1)))) (+ 1 np) (+ ns (length newpaths))))))))))))