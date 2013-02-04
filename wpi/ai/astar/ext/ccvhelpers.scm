
; fill a given grid with CCV's, 1 per row
(define (fill-ccv g)
  (let loop ((i 0))
    (if (< i (grid.height g))
        (begin 
          (grid-set! g (random (grid.width g)) i (make-cell 'C))
          (loop (+ i 1))))))

; get the posn of the ccv in given row
(define (get-ccv g r)
  (let loop ((i 0))
    (if (>= i (grid.width g)) #f
        (if (cell.ccv (grid-ref g i r))
            (make-posn i r)
            (loop (+ i 1))))))

; a method to check if there are conflicts in any direction, returns number of conflicts
;  Used to find number of conflicts each ccv has
(define (check-conflicts grid pos)
  (let ((c 0))
  ; check orthogonal's
    (if (check-conflict grid pos (make-posn 0 -1))
        (set! c (+ c 1)))
    (if (check-conflict grid pos (make-posn 0 1))
        (set! c (+ c 1)))
    (if (check-conflict grid pos (make-posn 1 0))
        (set! c (+ c 1)))
    (if (check-conflict grid pos (make-posn -1 0))
        (set! c (+ c 1)))
    
    ; check diagonal's
    (if (check-conflict grid pos (make-posn -1 -1))
        (set! c (+ c 1)))
    (if (check-conflict grid pos (make-posn -1 1))
        (set! c (+ c 1)))
    (if (check-conflict grid pos (make-posn 1 -1))
        (set! c (+ c 1)))
    (if (check-conflict grid pos (make-posn 1 1))
        (set! c (+ c 1)))
  
    c))

; a method to check if there is a conflict in a specific direction
; returns a bool
(define (check-conflict grid pos dir)
  (let loop ((pos pos) (dir dir))
    (let ((newpos (posn+ pos dir)))
      (if (not (valid-cell grid (posn.x newpos) (posn.y newpos))) #f
          (if (cell.ccv (grid-ref grid (posn.x newpos) (posn.y newpos)))
              #t
              (loop newpos dir))))))

; a method to display the CCV grid
(define (print-ccv-grid g)
  (let loop ((j 0))
    (if (< j (grid.height g))
        (begin
          (let loop ((i 0))
            (if (< i (grid.width g))
                (let ((val (cell.ccv (grid-ref g i j))))
                  (if val (printf "~a " val)
                      (printf "- "))
                  (loop (+ i 1)))))
          (printf "~n")
          (loop (+ j 1))))))