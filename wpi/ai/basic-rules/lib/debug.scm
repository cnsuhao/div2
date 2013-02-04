;;;  Project 0 debug module v1.0
;;;  Stores all printout methods for project 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    GENERAL   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "./data.scm")

; tell whether or not debug printing is on
(define debug #t)


; set debug printing
(define (debugmode b)
  (set! debug b))

; print if debug is set
(define-syntax debug-print
  (syntax-rules()
    ((debug-print a ...)
     (if debug (printf a ...)))))

; function to turn ' into '', required for bind templates
(define (double-quote x)
    (eval `'',x))   