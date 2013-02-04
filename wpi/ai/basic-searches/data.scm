;;;  The data module v1.0
;;;  Includes definitions for defnode, posn functions, and general grid functions

(module data mzscheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   NODES   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;;;  DEFNODE is a simple implementation of Structs.  Used instead of define-struct in case more functions need to be added and deeper control
;;   Use:  (defnode symbol (symbol ...))  where the symbols are the name of the node and its properties respectively
;;   Outcome, creates methods make-name, name?, name-eqv?, name.prop, and set!name.prop
;;       see posn for their use
(define-syntax defnode
  (syntax-rules()
    ((defnode n (prop ... ))
     ; initialize needed variables
     (let* ((name      (eval `',n))
            (name-s    (symbol->string name))
            (props     (list prop ...))
            (nprops    (length props)))
       
       ;; create make function
      (eval 
       `(define ,(string->symbol (string-append "make-" name-s))
          (lambda vals
            (let ((v (make-vector ,(+ nprops 1))))
              (vector-set! v 0 ',name)
              (let loop ((i 1) (vals vals))
                (if (< i (vector-length v))
                   (begin 
                     (vector-set! v i (car vals))
                     (loop (+ i 1) (cdr vals)))))
              v))))
       
       ;; create type function
       (eval 
       `(define ,(string->symbol (string-append name-s "?"))
          (lambda (v) 
            (if (vector? v)
             (let ((tmp (vector-ref v 0)))
               (and (symbol? tmp) (eqv? tmp ',name)))))))   
       
       ;; create type function
       (eval 
       `(define ,(string->symbol (string-append name-s "-eqv?"))
          (lambda (v1 v2) 
            (if (and (vector? v1) (vector? v2))
             (let ((l1 (vector->list v1))
                   (l2 (vector->list v2)))
               (let loop ((l1 l1) (l2 l2))
                 (cond [(and (null? l1) (null? l2)) #t]
                       [(or  (null? l1) (null? l2)) #f]
                       [(eqv? (car l1) (car l2)) (loop (cdr l1) (cdr l2))]
                       [else  #f])))))))
                       
       
       ;; create accessor functions
       (let loop ((i 1) (props props))
         (if (not (null? props)) (begin
           (eval 
            `(define ,(string->symbol (string-append name-s "." (symbol->string (car props))))
               (lambda (v) 
                 (if (vector? v)
                     (let ((tmp (vector-ref v 0)))
                          (if (and (symbol? tmp) (eqv? tmp ',name))
                              (vector-ref v ,i)))))))
           (loop (+ i 1) (cdr props)))))
       
       ;; create mutator functions
       (let loop ((i 1) (props props))
         (if (not (null? props)) (begin
           (eval 
            `(define ,(string->symbol (string-append "set!" (string-append name-s "." (symbol->string (car props)))))
               (lambda (v x) 
                 (if (vector? v)
                     (let ((tmp (vector-ref v 0)))
                          (if (and (symbol? tmp) (eqv? tmp ',name))
                              (vector-set! v ,i x)))))))
           (loop (+ i 1) (cdr props)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    POSN     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initially an expanded struct for all intents and purposes.  used to test defnode.
;; Now used to store x,y coordinates.  It remains expanded for module purposes
  
; make-posn num num -> vector
; creates a posn
(define (make-posn x y)
  (list->vector (list 'posn x y)))

; posn? posn -> bool
; returns if given vector is a posn
(define (posn? p)
  (eqv? (vector-ref p 0) 'posn))

; posn.x posn -> num
; returns x for posn
(define (posn.x p)
  (vector-ref p 1))

; posn.y posn -> num
; returns y for posn
(define (posn.y p)
  (vector-ref p 2))

; set!posn.x posn -> num
; sets x for posn
(define (set!posn.x p n)
  (vector-set! p 1 n))

; set!posn.y posn -> num
; sets y for posn
(define (set!posn.y p n)
  (vector-set! p 2 n))

; posn-eqv? posn posn -> bool
; checks if x and y are equal
(define (posn-eqv? p1 p2)
  (if (and (posn? p1) (posn? p2))
      (and (= (posn.x p1) (posn.x p2))
           (= (posn.y p1) (posn.y p2)))))

; posn-eqv? posn posn -> posn
; adds two posns together
(define (posn+ p1 p2)
  (make-posn (+ (posn.x p1) (posn.x p2))
             (+ (posn.y p1) (posn.y p2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(provide defnode
         make-grid grid-ref grid-set! grid.width grid.height grid.data
         make-posn posn? posn.x posn.y set!posn.x set!posn.y posn-eqv? posn+))