(include "../lib/grid.scm")
(include "../lib/rules.scm")      

; compare number of values to given n, returns true if same
(define (num-values n v)
  (if (not (list? (fact-value n))) #f
      (= (length (fact-value n)) v)))

; removes a value from a fact containing a list of values (no error handling)
(define (remove-from-fact n v)
  (let ((data (remove v (fact-value n))))
    (assert n is data)))

; removes a value from every fact
(define (remove-from-all v)
  (let loop ((ff facts))
    (if (not (null? ff))
        (begin
          (let ((data (remove v (cdar ff))))
            (assert (caar ff) is data))
          (loop (cdr ff))))))