(include "./data.scm")
(include "./debug.scm")

; a macro to create grids
; Use:  (defgrid (cellname (properties ...))
;                (((cell data)...)
;                 (...)))
;        cellname and properties create a node.  Each cell data entry creates and instance of the cell node.
; Returned:  Vector with ('grid width height data) where data is a vector table of cellnodes
(define-syntax make-grid
  (syntax-rules()
    ((make-grid (node props) (((d ...) ...) ... ))
     (let* ((cellnode (eval `',node))
            (data (list->vector (list 
                    (list->vector (list 
                      (list->vector (list cellnode d ...)) ...)) ...))))
       
       ; create the nodes
       (defnode cellnode props)
       
       ; return the grid
       (list->vector (list 'grid 
                           (vector-length (vector-ref data 0))
                           (vector-length data)
                           data))))))


; grid-ref: grid num num -> node
; returns the cell for a given x/y in a grid
; Note:  no out of bounds check
(define (grid-ref g x y)
  (vector-ref (vector-ref (vector-ref g 3) y) x))

; grid-set!: grid num num node -> void
; sets the cell for a given x/y in a grid
; Note:  no out of bounds check
(define (grid-set! g x y cell)
  (vector-set! (vector-ref (vector-ref g 3) y) x cell))

; grid?: grid -> bool
; returns whether g is a grid
; Note:  no error check
(define (grid? g)
  (eqv? 'grid (vector-ref g 0)))

; grid.width: grid -> int
; returns width of grid
; Note:  no guarentee g is a grid
(define (grid.width g)
  (vector-ref g 1))

; grid.heigth: grid -> int
; returns height of grid
; Note:  no guarentee g is a grid
(define (grid.height g)
  (vector-ref g 2))
  
; grid.data: grid -> vector
; returns the table of cells
; Note:  no guarentee g is a grid
(define (grid.data g)
  (vector-ref g 3))  


; valid-cell grid int int -> bool
;;  If cell is valid, it returns the cell, otherwise false (on grid and non obstacle/vehicle)
(define (valid-cell g x y)
  (begin 
    (if (or (or (< x 0) (>= x (grid.width g)))
            (or (< y 0) (>= y (grid.height g)))) #f (grid-ref g x y)))) ; out of bounds



; grid-index grid method value -> posn
;; grid-index takes a value, an accessor procedure, and a value (#f means not found)
(define (grid-index grid proc value)
  (let ((pos #f))
    (let loop ((y 0))
      (if (>= y (grid.height grid)) pos
          (begin 
            (let loop ((x 0))
              (if (< x (grid.width grid))
                  (if (eqv? value (proc (grid-ref grid x y)))
                      (set! pos (make-posn x y))
                    (loop (+ x 1)))))
           (if pos pos (loop (+ y 1))))))))



;; grid-copy grid -> grid
; copies given grid
(define (grid-copy g)
  (list->vector (list 'grid 
                      (grid.width g)
                      (grid.height g)
                      (copy-data (grid.data g)))))

;; copy-data data -> data
; remakes the grids data by converting all internal vectors to lists and back
(define (copy-data data)
  (list->vector 
   (map (lambda (r) 
          (list->vector 
           (map (lambda (c) 
                  (list->vector (vector->list c))) 
                (vector->list r))) )
        (vector->list data))))



; get-adjacent grid posn -> list (posn cell)
; gets positions of cells next to posn
(define (get-adjacent grid pos)
  (let ((x (posn.x pos))
        (y (posn.y pos)))
  ; filter out the false neighbors
  (filter (lambda(p) p)
          ; make list
          (list (if (valid-cell grid (- x 1) y)
                    (make-posn (- x 1) y) #f)
                (if (valid-cell grid (+ x 1) y)
                    (make-posn (+ x 1) y) #f)
                (if (valid-cell grid x (- y 1))
                    (make-posn x (- y 1)) #f)
                (if (valid-cell grid x (+ y 1))
                    (make-posn x (+ y 1)) #f)))))

; get-adjacent/dir grid posn -> list (posn cell)
; gets directions of cells next to posn
(define (get-adjacent/dir grid pos)
  (map (lambda(p) (posn- p pos)) (get-adjacent grid pos)))

; grid-swap posn posn -> void
; swap values from two posns (no error check)
(define (grid-swap grid pos dir)
  (let* ((curr  (grid-ref grid (posn.x pos) (posn.y pos)))
        (newpos (posn+ pos dir))
        (next   (grid-ref grid (posn.x newpos) (posn.y newpos))))
    (grid-set! grid (posn.x pos) (posn.y pos) next)
    (grid-set! grid (posn.x newpos) (posn.y newpos) curr)))

; get the opposite of given direction
(define (opposite-dir dir)
  (make-posn (- (posn.x dir)) (- (posn.y dir))))
                     