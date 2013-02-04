(include "../lib/grid.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; find neighbors which are not blocked
(define (get-neighbors grid pos)
  (filter (lambda(p) 
            (let ((cell (grid-ref grid (posn.x p) (posn.y p))))
              (and (= 9 (cell.veh cell))
                   (= 0 (cell.child cell))
                   (not (cell.obs cell)))))
          (get-adjacent grid pos)))

; print grid for GA
(define (print-grid g)
  (let loop ((j 0))
    (if (< j (grid.height g))
        (begin
          (let loop ((i 0))
            (if (< i (grid.width g))
                (let ((veh   (cell.veh   (grid-ref g i j)))
                      (obs   (cell.obs   (grid-ref g i j)))
                      (child (cell.child (grid-ref g i j))))
                  (cond [(< veh 9)   (debug-print "~a " veh)]
                        [(> child 0) (debug-print "~a " (if (= 1 child) 'a 'b))]
                        [obs         (debug-print "X ")]
                        [else        (debug-print "- ")])
                  (loop (+ i 1)))))
          (debug-print "~n")
          (loop (+ j 1))))))