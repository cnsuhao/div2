(include "./lib/data.scm")
(include "./lib/debug.scm")
(include "./lib/grid.scm")

(include "./ext/gridhelpers.scm")

; basic goal of a 3x3
(define goal
  (make-grid ('cell ('truck))
    (((1) (2) (3))
     ((4) (5) (6))
     ((7) (8) (#f)))))

; provided shuffled grid
(define test1 
  (make-grid ('cell ('truck))
    (((7) (4) (2))
     ((8) (1) (3))
     ((5) (6) (#f)))))

; self made shuffled grid
(define test2 
  (make-grid ('cell ('truck))
    (((3) (5) (2))
     ((8) (6) (7))
     ((1) (4) (#f)))))


; computer made shuffled grid
(define test3 (grid-copy goal))
(shuffle-grid test3 100)

; same as before, 10 times shufle
(define test4 (grid-copy goal))
(shuffle-grid test4 1000)


; basic goal of a 4x4
(define goal4x4
  (make-grid ('cell ('truck))
    (((1) (2) (3) (4))
     ((5) (6) (7) (8))
     ((9) (10) (11) (12))
     ((13) (14) (15) (#f)))))

; basic 4x4 test
(define test4x4 (grid-copy goal4x4))
(shuffle-grid test4x4 500)

; basic goal of a 6x6
(define goal6x6
  (make-grid ('cell ('truck))
    (((1) (2) (3) (4) (5) (6))
     ((7) (8) (9) (10) (11) (12))
     ((13) (14) (15) (16) (17) (18))
     ((19) (20) (21) (22) (23) (24))
     ((25) (26) (27) (28) (29) (30))
     ((31) (32) (33) (34) (35) (#f)))))

; basic 6x6 test
(define test6x6 (grid-copy goal6x6))
(shuffle-grid test6x6 500)




;;;;;;   STATE
;  A state is defined as 
;    (list estimate-left distance-moved previous-dir grid)
;;;;;;
; get the esitmate-left
(define (state.estimate state)
  (car state))

; get distance moved
(define (state.dist state)
  (cadr state))

; get previous dir
(define (state.dir state)
  (caddr state))

; get the state's grid
(define (state.grid state)
  (cadddr state))

; copy a given state
(define (state-copy state)
  (list (state.estimate state)
        (state.dist state)
        (state.dir state)
        (grid-copy (state.grid state))))

; get the posn of the empty cell
(define (state-get-empty state)
  (grid-index (state.grid state) cell.truck #f))

; fetch given state from a state list (returns null if not found)
(define (get-state ss s)
  (let ((grid (state.grid s)))
    (if (null? ss) ()
        (if (grid-eqv? (state.grid (car ss))
                       (state.grid s))
            (car ss)
            (get-state (cdr ss) s)))))





;;;;;;;;;;;    GRID HELPERS   ;;;;;;;;;;;;;

; uses manhatten distance for each item squared (meaning the closer an individual square is, the better)
; (sum of distn^2)
; estimate-to-goal grid grid -> int
(define (estimate-to-goal curr goal)
  ; loop through grid cells
    (let loop ((y 0))
         (if (>= y (grid.height curr)) 0
             (+ (let loop ((x 0))
                  (if (>= x (grid.width curr)) 0
                      ; if value isn't false
                      (let ((currval (cell.truck (grid-ref curr x y))))
                        (if (not currval) (loop (+ x 1))
                            ; get total dist
                            (let* ((delta (posn- (make-posn x y)    
                                                 (grid-index goal cell.truck currval))); get same value in goal
                                   (dist (expt (+ (abs (posn.x delta)) (abs (posn.y delta))) 2)))
                              (+ dist (loop (+ x 1))))))))
             (loop (+ y 1))))))




;;;;;;;;;;;    MAIN FUNCTION  ;;;;;;;;;;;;;

; sort trucks in ascending order for provided grid
(define (sort-trucks start goal)
  (let* ((state (list (estimate-to-goal start goal) 0 #f start))
         (empty-pos (state-get-empty state))
         (open-states (list state)) ; estimate-left, distance-moved, previous-dir, and state
         (closed-states null)
         (c 0))  ; counter
    
    
    (let loop()
      ;; get current state
      (set! c (+ c 1))
      (set! state (car open-states))
      (set! empty-pos (state-get-empty state))
      
      ; Debug printing
      (debug-print "~n***********")
      (debug-print "~nSteps:  ~a~nDistance:  ~a~nEstimate:  ~a~nTotal:  ~a~n" c (state.dist state) (state.estimate state) 
                                                                           (+ (state.dist state) (state.estimate state)))
      (debug-print "Open-States:  ~a~nClosed-States:  ~a~n~n" (length open-states) (length closed-states))
      (if (state.dir state)
          (let* ((newpos (posn+ empty-pos (state.dir state)))
                 (val (cell.truck (grid-ref (state.grid state) (posn.x newpos) (posn.y newpos)))))
            (debug-print "Moved:  ~a to ~a,~a~n" val (posn.x newpos) (posn.y newpos))))
      (print-grid (state.grid state) debug)
      
      
      ;; are we at the goal?
      (if (not (zero? (state.estimate state)))
         ; continue finding path
        (begin

          ; move current state to closed
          (set! open-states   (remove state open-states))
          (set! closed-states (cons state closed-states))
             
          ; find new to open-states
          (let* ((dirs (get-adjacent/dir (state.grid state) empty-pos)) ; adjacent directions
                 (dist (state.dist state))
                 (dir+grid (map (lambda (d) (cons d (grid-copy (state.grid state)))) dirs)) ; create list of dirs and grid-copys
                 (swapped (map (lambda (p) (grid-swap (cdr p) empty-pos (car p)) p) dir+grid)) ; swap in given dir
                 ; create new states
                 (states (map (lambda (p)
                                (list (estimate-to-goal (cdr p) goal) 
                                      (+ dist 1) 
                                      (opposite-dir (car p))
                                      (cdr p))) 
                              swapped)))
                  
            ;;;; handle conflicts
            ;; remove whichever state has a further distance to travel from list
            ; Check closed states
            (let loop ((ss states))
              (if (not (null? ss))
                  (let ((s1 (car ss))
                        (s2 (get-state closed-states (car ss))))
                    ;; if we found a duplicate in open, compare
                    (if (not (null? s2))
                        (begin 
                          (set! states (remove s1 states))
                          (debugmode (not (posn-eqv? (state.dir s1) (opposite-dir (state.dir state)))))
                          (debug-print "--Found duplicate closed state in ~a, ~a direction~n" (posn.x (state.dir s1)) (posn.y (state.dir s1)))
                          (if (< (state.dist s1) (state.dist s2))
                              ; add state to closed
                              (begin (debug-print "----Kept new state~n")
                                     (set! closed-states (cons s1 (remove s2 closed-states))))
                              (debug-print "----Kept old state~n"))))    
                    (loop (cdr ss)))))
            
            ; Check open states
            (let loop ((ss states))
              (if (not (null? ss))
                  (let ((s1 (car ss))
                        (s2 (get-state open-states (car ss))))
                    ;; if we found a duplicate in open, compare
                    (if (not (null? s2))
                        (begin 
                          (set! states (remove s1 states))
                          (debugmode (not (posn-eqv? (state.dir s1) (state.dir state))))
                          (debug-print "--Found duplicate closed state in ~a, ~a direction~n" (posn.x (state.dir s1)) (posn.y (state.dir s1)))
                          (if (< (state.dist s1) (state.dist s2))
                              ; add state to open
                              (begin (debug-print "----Kept new state~n")
                                     (set! open-states (cons s1 (remove s2 open-states))))
                              (debug-print "----Kept old state~n"))))
                    (loop (cdr ss)))))
            
            (debugmode #t)
            
            ;;  Add states      
            (set! open-states (append states open-states))
            (debug-print "--Added ~a states~n" (length states)))
          
          ; sort all open states for lowest estimate
          (set! open-states
                (sort open-states
                      (lambda (a b)
                        (< (+ (state.estimate a) (state.dist a))
                           (+ (state.estimate b) (state.dist b))))))
          (loop))
          
        
        
        ;;;;; else, ending sequence
        ;; build path 
        (let ((path null) (strt (grid-copy start)))
          (set! path
            (let loop ((dir (state.dir state)) (s state))
              (if (not dir) ()
                  (begin
                    (set! closed-states (remove s closed-states))
                    (grid-swap (state.grid s) (state-get-empty s) dir)
                    (cons dir (loop (state.dir (get-state closed-states s)) (get-state closed-states s)))))))
        
          ;; print path
          (printf "~n~n~n")
          (printf "~n***************************************~n")
          (printf "           Solution Found!")
          (printf "~n***************************************~n")
          
          (set! c 0)
          
          (let loop ((pp (reverse path)))
            (set! c (+ c 1))
            (printf "Step: ~a~n~n" c)
            (if (null? pp) (begin (print-grid strt #t) (printf "~nDone!~n~n"))
                (begin                 
                  (set! empty-pos (grid-index strt cell.truck #f))
                  (print-grid strt #t)
                  (printf "~nMove ~a from ~a,~a to ~a,~a~n~n"  
                          (cell.truck (grid-ref strt (posn.x (posn+ empty-pos (opposite-dir (car pp))))
                                                     (posn.y (posn+ empty-pos (opposite-dir (car pp))))))
                          (posn.x (posn- empty-pos (car pp))) (posn.y (posn- empty-pos (car pp)))
                          (posn.x empty-pos) (posn.y empty-pos))
                  (grid-swap strt empty-pos (opposite-dir (car pp)))
                  (printf "**********************~n")
                  (loop (cdr pp))))))))))