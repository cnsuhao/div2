(include "../lib/grid.scm")
(include "../lib/rules.scm")

;  Create a fact for each cell in a grid with a given value.  
;     This is specific to the task two problem so requires a list of vehicle options and elevation options
(define (generate-facts grid vv ee)
  (let loop ((i 0))
    (if (< i (grid.width grid))
        (begin 
          (let loop ((j 0))
            (if (< j (grid.height grid))
                (begin (deffacts 
                         ((generate-cell "cell" i j 'veh) is vv)
                         ((generate-cell "cell" i j 'elv) is ee))
                       (loop (+ j 1)))))
          (loop (+ i 1))))))


;  Generate a cell name as a string from given values, formate is cellXxY.type
(define (generate-cell cell x y type)
  (let ((coords (string-append (number->string x) (string-append "x" (number->string y)))))
    (string->symbol (string-append cell (string-append coords (string-append "." (symbol->string type)))))))
