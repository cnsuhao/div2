(include "./data.scm")
(include "./debug.scm")

; a macro to create grids
; Use:  (defgrid (cellname (properties ...))
;                (((cell data)...)
;                 (...)))
;        cellname and properties create a node.  Each cell data entry creates and instance of the cell node.
; Returned:  Vector with ('grid width height data) where data is a vector table of cellnodes
(define-syntax make-grid
  (syntax-rules(n)
    ((defgrid (node props) (((d ...) ...) ... ))
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

; grid-ref: grid num num node -> void
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

