(include "../lib/grid.scm")


;  check whether or not all cells in the landscape are set (completion condition)
(define (grid-filled _1 _2)
  (let ((filled #t))
    (let loop ((i 0))
      (if (>= i (grid.width final-grid)) filled
          (begin (let loop ((j 0))
                   (if (< j (grid.height final-grid))
                       (begin (let ((cell (grid-ref final-grid i j)))
                                (set! filled (and filled (symbol? (cell.veh cell))
                                                  (not (= 0 (cell.elv cell))))))
                              (loop (+ j 1)))))
                 (loop (+ i 1)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; a function to see if there is a valid cell with given value to the direction of given cell
(define (next-to cell dir v)
  (let* ((cell (symbol->string cell))
         (pos (get-coords cell))
         (newpos (posn+ pos dir))
         (celltype (if (symbol? v) 'veh 'elv))
         (newcell (generate-cell "cell" (posn.x newpos) (posn.y newpos) celltype)))
    (if (valid-cell final-grid (posn.x newpos) (posn.y newpos))
        (cell-contains newcell v)
        #f)))

; instances of next-to for each direction
(define (north-of cell v)
  (next-to cell (make-posn 0 1) v))

(define (south-of cell v)
  (next-to cell (make-posn 0 -1) v))

(define (east-of cell v)
  (next-to cell (make-posn -1 0) v))

(define (west-of cell v)
  (next-to cell (make-posn 1 0) v))

(define (is-on cell v)
  (next-to cell (make-posn 0 0) v))



;  A method to get the number of neighbors a square has
(define (num-neighbors cell v)
    (let ((num (length (listneighbors (get-coords (symbol->string cell)) 
                                      (list (make-posn -1 0)
                                            (make-posn 1 0)
                                            (make-posn 0 -1)
                                            (make-posn 0 1))))))
      (= num v)))
    
; A helpder function for num-neighbors, returns list of valid neighbors
(define (listneighbors pos dirs)
  (if (null? dirs) ()
      (let ((newpos (posn+ pos (car dirs))))
        (if (valid-cell final-grid (posn.x newpos) (posn.y newpos))
            (cons newpos (listneighbors pos (cdr dirs)))
            (listneighbors pos (cdr dirs))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; check whether or not the fact of a cell contains the given value
(define (cell-contains n v)
  (let* ((vv (fact-value n)))
    (if (not (list? vv)) #f    
        (not (null? (filter (lambda(x) (eqv? x v)) vv))))))

; retrieve the coords from a cell name
(define (get-coords cell)
  (let ((x (string->number (list->string (list (string-ref cell 4)))))
        (y (string->number (list->string (list (string-ref cell 6))))))
    (make-posn x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-grid g)
  (let loop ((j 0))
    (if (< j (grid.height g))
        (begin
          (let loop ((i 0))
            (if (< i (grid.width g))
                (let ((veh (cell.veh (grid-ref g i j))))
                  (if veh (printf "~a " veh)
                      (printf "- "))
                  (loop (+ i 1)))))
          (printf "     ")
          (let loop ((i 0))
             (if (< i (grid.width g))
                 (let ((elv (cell.elv (grid-ref g i j))))
                   (if (> elv 0) (printf "~a " elv)
                       (printf "- "))
                   (loop (+ i 1)))))
          (printf "~n")
           (loop (+ j 1))))))