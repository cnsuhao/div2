(include "./lib/data.scm")
(include "./lib/debug.scm")
(include "./lib/grid.scm")

(include "./ext/ccvhelpers.scm")

; an empty 8x8 board
(define board1 (make-grid
   ('cell ('ccv))
   (((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f)))))

; an empty 12x12 board
(define board2 (make-grid
   ('cell ('ccv))
   (((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f))
    ((#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f) (#f)))))

; place the ccv's in the board
(fill-ccv board1)
(fill-ccv board2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; method to resolve all conflicts and solve the 8 queens puzzle
(define (check-ccv grid n)
  (print-ccv-grid grid)
  (let ((c 0) (t 0))  ;; c = how many times there has been no change in number of conflicts, t = num conflicts
    
    ;;  Loop from i to n
    (let loop((i 0))
      (if (< i n)
          ; get the positions of the ccvs and number of conflicts per
          (let* ((ccvs (let loop ((i 0))
                         (if (>= i (grid.height grid)) ()
                             (cons (get-ccv grid i)
                                   (loop (+ i 1))))))
                 (conflicts (map (lambda (p) (cons p (check-conflicts grid p))) ccvs)))
       
            
            ; debug
            (printf "********************~n")
            (if (= t (foldr (lambda (a b) (+ a b)) 0 (map cdr conflicts)))
                (set! c (+ c 1))
                (set! c 0))
            (set! t (foldr (lambda (a b) (+ a b)) 0 (map cdr conflicts)))
            (printf "Num Conflicts:  ~a~n" t)
            (set! conflicts (filter (lambda(c) (not (= 0 (cdr c)))) conflicts))
            
            
            ; check if complete
            (if (not (null? conflicts))
                ;;;; not done, keep going
                ;;  Get options
                (let* ((ccv   (caar (shuffle-list conflicts 30)))
                       (posns (map (lambda(x) (make-posn x (posn.y ccv))) (range 0 (grid.width grid))))
                       (row  (map (lambda (p) (cons p (check-conflicts grid p))) posns)))                  
                  
                  ; sort by min conflicts and shuffle those with same value
                  ; (select one at random once width^2 tries to attempt to clear lockup
                  (if (> c (expt (grid.height grid) 2))
                      (set! row (shuffle-list row 30))
                      (set! row (sort row (lambda(a b) (< (cdr a) (cdr b))))))
                  ; filter down to a couple of values
                  (set! row (filter (lambda(a) (= (cdr a) (cdar row))) row))
                  (set! row (shuffle-list row 5))          
                  
                  ; swap the new cell with the old
                  (let ((val (cell.ccv (grid-ref grid (posn.x ccv) (posn.y ccv)))))
                    (grid-set! grid (posn.x ccv) (posn.y ccv) (make-cell #f))
                    (printf "~a, ~a to " (posn.x ccv) (posn.y ccv))
                    (grid-set! grid (posn.x (caar row)) (posn.y (caar row)) (make-cell val))
                    (printf "~a, ~a~n" (posn.x (caar row)) (posn.y (caar row))))
                  
                  
                  ;; print grid
                  (print-ccv-grid grid)
                  
                  (loop (+ i 1)))
                (print-ccv-grid grid)))))))