(include "../lib/grid.scm")

; a method to tell if two grids are equal
(define (grid-eqv? g1 g2)
  (let loop ((j 0))
    (if (>= j (grid.height g1)) #t
        (and (let loop ((i 0))
               (if (>= i (grid.width g1)) #t
                   (and (eqv? (cell.truck (grid-ref g1 i j))
                              (cell.truck (grid-ref g2 i j)))
                        (loop (+ i 1)))))
             (loop (+ j 1))))))

; a method to mix up grids
; shuffle-grid grid num -> void
(define (shuffle-grid grid n)
  (let loop ((i 0))
    (let ((empty (grid-index grid cell.truck #f)))
      (if (< i n)
          (begin
            (grid-swap grid empty (car (shuffle-list (get-adjacent/dir grid empty) 20)))
            (loop (+ i 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; print grid for A* truck sort
(define (print-grid g t)
  (let ((tmp debug))
    (debugmode t)
    (let loop ((j 0))
      (if (< j (grid.height g))
          (begin
            (let loop ((i 0))
              (if (< i (grid.width g))
                  (let ((val (cell.truck (grid-ref g i j))))
                    (if val 
                        (if (< val 10)
                            (debug-print "0~a " val)
                            (debug-print "~a " val))
                        (debug-print "-- "))
                    (loop (+ i 1)))))
            (debug-print "~n")
            (loop (+ j 1)))))
    (debugmode tmp)))