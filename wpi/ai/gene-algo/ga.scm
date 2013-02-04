;;;;;;;;;;;;;;;
;;   http://web.cs.wpi.edu/~dcb/courses/CS4341/2007/project3.html
;;;;;;;;;;;;;;;

(include "./lib/data.scm")
(include "./lib/debug.scm")
(include "./lib/grid.scm")

(include "./ext/searchteam.scm")
(include "./ext/gridhelpers.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;              DATA            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Search team attributes
;;
;;    Type 	   Distance    See     Hear
;;    Sprinter 	       8 	2 	8
;;    Walker           6 	5 	6
;;    Crawler 	       4 	6 	5
;;    Sitter 	       3 	8 	2
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-attribute sprinter distance 8)
(set-attribute sprinter sight    2)
(set-attribute sprinter hearing  8)

(set-attribute walker distance 6)
(set-attribute walker sight    5)
(set-attribute walker hearing  6)

(set-attribute crawler distance 4)
(set-attribute crawler sight    6)
(set-attribute crawler hearing  5)

(set-attribute sitter distance 3)
(set-attribute sitter sight    8)
(set-attribute sitter hearing  2)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Landscape
;;
;;    - - X - - - 1 - - - 
;;    - - - - - - X - 2 - 
;;    - - - - - - X X - - 
;;    - - - - - - - - - - 
;;    - - - - - - - - - - 
;;    - - - - - - - - - - 
;;    - - - - - - - - X - 
;;    - - - - - - - - - - 
;;    - - - - - - - - - - 
;;    - - - - - - - - - - 
;;
;;       X = obstacle
;;       1/2 = children (numbered)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define landscape (make-grid
  ('cell ('veh 'child 'obs))
  (((9 0 #f) (9 0 #f) (9 0 'X) (9 0 #f) (9 0 #f) (9 0 #f) (9 1 #f) (9 0 #f) (9 0 #f) (9 0 #f))
   ((9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 'X) (9 0 #f) (9 2 #f) (9 0 #f))
   ((9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 'X) (9 0 'X) (9 0 #f) (9 0 #f))
   ((9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f))
   ((9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f))
   ((9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f))
   ((9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 'X) (9 0 #f))
   ((9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f))
   ((9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f))
   ((9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f) (9 0 #f)))))

(define child1pos (make-posn 6 0))
(define child2pos (make-posn 8 1))

(define vehstart (make-posn 0 9))

(print-grid landscape)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Initial team
;;
;;   Sprinter    0
;;   Walker      0
;;   Crawler     0
;;   Sitter      1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define initial-team (vector 0 0 0 1))
(define previousteams (list initial-team))
(define currentteams  (list initial-team))












;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;          MUTATION            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mutate-team t)
  (let ((team (copy-team t))
        (type (random 4))
        (amt  (random 3)))
    (vector-set! team type amt)
    (if (not (and (legal-team team)
                  (not (team-eqv? team t))))
        (mutate-team t)
        team)))

;; returns crossover between team1 and team2
(define (crossover-team t1 t2)
  (let loop ((crosspts (shuffle-list (range 1 4) 10)))
    ; if there is no answer, return ()
    (if (null? crosspts) ()
    ; check if this pt yields a legal shuffle
    (let ((team1 null)
          (team2 null)          
          (h1 null)
          (h2 null)
          (t1 (vector->list (copy-team t1)))
          (t2 (vector->list (copy-team t2)))
          (pt (car crosspts)))
      (let loop ((i 0))
        (if (< i pt)
            (begin
              (set! h1 (append h1 (list (car t1))))
              (set! h2 (append h2 (list (car t2))))
              (set! t1 (cdr t1))
              (set! t2 (cdr t2))
              (loop (+ i 1)))
            (begin
              (set! team1 (list->vector (append h1 t2)))
              (set! team2 (list->vector (append h2 t1))))))
      (if (and (legal-team team1) (legal-team team2))
          (list team1 team2)
          (loop (cdr crosspts)))))))
            
      


;; returns all new crossovers between t and the remainder
(define (crossover-all t lt)
  (let loop ((lt lt))
    (if (null? lt) ()
        (append (crossover-team t (car lt))
                (loop (cdr lt))))))

;; check if team is legal (one member, and not 4 types)
(define (legal-team t)
  (let ((team (vector->list t)))
  ; not legal if there are no teams
  (if (zero? (foldr + 0 (vector->list t)))
      #f
      ; find a legal team
      (let loop ((team team))
        (if (null? team) #f
            (if (zero? (car team)) #t
                (loop (cdr team))))))))


;; code to generate a new generation
(define (create-new-teams lt)
  (append
   ;; mutators
   (let loop ((lt lt))
    (if (null? lt) ()
        (cons (mutate-team (car lt))
                (loop (cdr lt)))))
    ;; crossover
   (let loop ((lt lt))
     (if (null? lt) ()
         (append (crossover-all (car lt) (cdr lt))
                 (loop (cdr lt)))))))

;; removes all items that exist in global list from passed list
(define (remove-dup-teams lt)
  (let loop ((all previousteams)
             (newteams lt))
    (if (null? all) newteams
        (loop (cdr all) (filter (lambda(t) (not (team-eqv? (car all) t))) newteams)))))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;          Movement            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; moves team on grid, returns a list of veh positions
(define (move-team grid team)
  (let ((team (copy-team team)))
    ;; if there are vehicles left
    (if (zero? (foldr + 0 (vector->list team))) null
    ;; find a new type
    (let ((type (random 4))
          (move null))
      ;; if none of that type left
      (if (not (zero? (vector-ref team type)))
          ; otherwise move the  veh
          (begin
            ; decrease that tyeam by one
            (vector-set! team type (- (vector-ref team type) 1))
            ; find a valid move
            (let loop ()
              ;; move the vehicle, check if it is valid and either update or move again
              (set! move (move-veh grid type))
              (if move
                  (let ((cell (grid-ref grid (posn.x move) (posn.y move))))
                      ;; if veh is on an illegal location
                      (if (and (not (cell.obs cell)) (= (cell.veh cell) 9))
                          (set!cell.veh cell type)
                          (loop)))))
            (cons move (move-team grid team)))
          (move-team grid team))))))

            

;; Move an individual vehicle randomly, return final position
(define (move-veh grid type)
  (let loop ((move (get-attribute type distance))
             (pos vehstart))
    ; if we are out of moves, send back position
    (if (zero? move) pos       
        ;; move in a random dir
        (loop (- move 1) (posn+ pos (random-dir grid pos))))))
                

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Choosing movement directions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; how to decide a random-direction, shuffles a list of indecies based on which directions are valid
(define (random-dir grid pos)
  (let* ((dirs (map (lambda (p) (posn- p pos)) (get-adjacent grid pos)))
         (prob-index (generate-prob dirs)))
    (vector-ref (list->vector dirs) (car (shuffle-list prob-index 30)))))

;; generates a list of indicies
(define (generate-prob dirs)
  (let loop ((i 0) (dirs dirs))
    (if (null? dirs) ()
      (let ((dir (car dirs)))
        (append
         ;; for each type of dir, weight them by adding more instances of their index
         (cond [(and (= 1 (posn.x dir)) (= -1 (posn.y dir)))
                (list i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i)]
               [(or (and (= 1 (posn.x dir)) (=  0 (posn.y dir)))
                    (and (= 0 (posn.x dir)) (= -1 (posn.y dir))))
                (list i i i i i i i i i i)]
               [(or (and (= -1 (posn.x dir)) (= -1 (posn.y dir)))
                    (and (=  1 (posn.x dir)) (=  1 (posn.y dir))))
                (list i i i)]
               [(or (and (= -1 (posn.x dir)) (=  0 (posn.y dir)))
                    (and (=  0 (posn.x dir)) (=  1 (posn.y dir))))
                (list i i)]
               [else (list i)])
         (loop (+ i 1) (cdr dirs)))))))
 
                 

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Sensors (seeing/hearing)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return whether or not vehicle can see child
(define (can-see grid cpos pos type)
  (can-sense grid cpos pos (get-attribute type sight) #f))
   
; return whether or not vehicle can hear child
(define (can-hear grid cpos pos type)
    (can-sense grid cpos pos (get-attribute type hearing) #t))

;; checks if vehicle can sense the child from given position
(define (can-sense grid cpos pos amt noblock)
  ;; move through grid to determine if player can be seen
  (let loop ((step amt)
             (pos pos))
    (if (= step 0) #f
        ; get the next direction
        (let ((cell (grid-ref grid (posn.x pos) (posn.y pos)))
              (dir (child-dir pos cpos)))
          (set! pos (posn+ pos dir))
          ; if the same place as the child, return true
          (if (posn-eqv? pos cpos) #t
              ; cell is unblocked
              (if (or noblock
                      (and (= 9 (cell.veh cell))
                           (cell.obs cell)
                           (= 0 cell.child)))
                  (loop (- step 1) pos)
                  #f))))))


;;;;;; Method to get the movement at each step of checking for sight or hearing
;;
;;IF xd = xc AND yd = yc THEN child found. 
;;IF xd = xc THEN move vertically. 
;;IF yd = yc THEN move horizontally. 
;;IF (xd - xc) approx equal (yd - yc) THEN move diagonally. 
;;IF (xd - xc) greater than (yd - yc) THEN move horizontally. 
;;IF (xd - xc) less than (yd - yc) THEN move vertically. 
(define (child-dir pos child)
  (cond [(and (= (posn.x pos) (posn.x child)) (= (posn.y pos) (posn.y child))) (make-posn 0 0)]
        [(= (posn.x pos) (posn.x child)) 
         (make-posn 0 (/ (- (posn.y child) (posn.y pos)) (abs (- (posn.y child) (posn.y pos)))))]
        [(= (posn.y pos) (posn.y child)) 
         (make-posn (/ (- (posn.x child) (posn.x pos)) (abs (- (posn.x child) (posn.x pos)))) 0)]
        [(and (<= (- (abs (- (posn.x pos) (posn.x child))) (abs (- (posn.y pos) (posn.y child)))) 1)
              (>= (- (abs (- (posn.x pos) (posn.x child))) (abs (- (posn.y pos) (posn.y child)))) -1))
         (make-posn (/ (- (posn.x child) (posn.x pos)) (abs (- (posn.x child) (posn.x pos))))
                    (/ (- (posn.y child) (posn.y pos)) (abs (- (posn.y child) (posn.y pos)))))]
        [(> (abs (- (posn.x pos) (posn.x child))) (abs (- (posn.y pos) (posn.y child))))
         (make-posn (/ (- (posn.x child) (posn.x pos)) (abs (- (posn.x child) (posn.x pos)))) 0)]
        [(<= (abs (- (posn.x pos) (posn.x child))) (abs (- (posn.y pos) (posn.y child))))
         (make-posn 0 (/ (- (posn.y child) (posn.y pos)) (abs (- (posn.y child) (posn.y pos)))))]))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;          Evaluation          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;   Test team

;;place at starting point
;;move pseudo randomly
(define (evaluate-teams tt c)
  (let loop ((tt tt))
    (if (null? tt) ()
        (cons (/ (evaluate-team (car tt) 5 c) 5)
              (loop (cdr tt))))))

;; get the average of the total team score per round
(define (evaluate-team t n c)
  (let loop ((i 0))
       (if (= i n) 0
           (let* ((team  t)
                  (grid  (grid-copy landscape))
                  (vehs  (move-team grid t))
                  (score (team-score grid vehs)))
             (if (< c 2) (begin
                           (debug-print "--------------------------~n")
                           (print-grid grid)
                           (debug-print "~nTeam:  ")
                           (print-team team)
                           (debug-print "~nTeam Score:  ~a~n~n" score)))
             (+ score
                (loop (+ i 1)))))))

;; return the total score for the team
(define (team-score grid vehs)
  (let loop ((vehs vehs))
    (if (null? vehs) 0
        (+ (veh-score grid (car vehs))
           (loop (cdr vehs))))))

;; get the individual score for a vehicle
(define (veh-score grid veh)
  (+ (if (can-see  grid child1pos veh (cell.veh (grid-ref grid (posn.x veh) (posn.y veh)))) 10 0)
     (if (can-see  grid child2pos veh (cell.veh (grid-ref grid (posn.x veh) (posn.y veh)))) 10 0)
     (if (can-hear grid child1pos veh (cell.veh (grid-ref grid (posn.x veh) (posn.y veh)))) 6 0)
     (if (can-hear grid child2pos veh (cell.veh (grid-ref grid (posn.x veh) (posn.y veh)))) 6 0)))


;; calculate the diversity of the teams, return as a list
(define (calculate-diversity pool)
  (let loop ((teams pool))
    (if (null? teams) ()
        (cons (list (caar teams) (cadr (car teams)) 
              (diversity-rank (caar teams)))
              (loop (cdr teams))))))

;; find the diversity based off of current teams
(define (diversity-rank team)
  (let loop ((tt currentteams))
    (if (null? tt) 0
        (let ((n (sqr (distance-teams (car tt) team))))
        (+ (if (zero? n) 0 (/ 1 n))
           (loop (cdr tt)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;          Main                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run)
  (let loop ((c 0) (quality 0))
    (debug-print "-=-=-=-=-=-=-=-=-=-=-=-~n")
    (debug-print "Generation: ~a ~n" c)
    (debug-print "Quality: ~a ~n~n" quality)
    
    ;; stop when quality of search is greater than 54
    (if (<= quality 54) (begin
    
    (debug-print "Current Teams: ~n")
    (print-teams currentteams)
    ;; get new teams
    (let ((newteams (remove-dup-teams (create-new-teams currentteams))))
      (set! currentteams (append currentteams newteams))
      (set! previousteams (append previousteams newteams))
      (debug-print "New Teams: ~n")
      (print-teams newteams))
    
    ;; evaluate each team 
    (let ((pool (let loop ((ct currentteams) (qt (evaluate-teams currentteams c)))
                  (if (null? ct) ()
                      (cons (list (car ct) (car qt) 0) (loop (cdr ct) (cdr qt))))))
          (kept null))
      
      (debug-print "         Team     Quality   Diversity~n")
      ; sort by quality
      (set! pool (sort pool (lambda(x y) (> (cadr x) (cadr y)))))
      ; set the first to the currentteams
      (set! currentteams (list (caar pool)))
      (set! quality (cadr (car pool)))
      
      (debug-print "Team0:  ")
      (print-team (caar pool))
      (debug-print "      ~a       " (cadr (car pool)))
      (debug-print "0~n")
      
      (set! pool (cdr pool))
      
      ; add the next three
      (let loop ((c 0) (pool pool))
        (if (not (null? pool))
            (begin
              ; calculate diversity and sort
              (set! pool (calculate-diversity pool))
              (set! pool (sort pool (lambda(x y) (if (= (+ (cadr x) (caddr x)) (+ (cadr y) (caddr y)))
                                                     (> (caddr x) (caddr y))
                                                     (> (+ (cadr x) (caddr x)) (+ (cadr y) (caddr y)))))))
              ; set the next to currentteams (if one of the first 3
              (if (< c 3)
                  (set! currentteams (append currentteams (list (caar pool)))))
              
              (debug-print "Team~a:  " (+ 1 c))
              (print-team (caar pool))
              (debug-print "      ~a       " (cadr (car pool)))
              (debug-print "~a~n" (caddr (car pool)))
              
              ; add the next
              (loop (+ c 1) (cdr pool))))))
    
    ; print which teams were kept and with what score
    (loop (+ c 1) quality))
    
    
    ;;;;;; END OF LOOP
    (begin
      (debug-print "Final Team: ~n")
      (print-team (car currentteams))
      (debug-print "~n~n")))))
