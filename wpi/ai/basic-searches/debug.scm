;;;  Project 0 debug module v1.0
;;;  Stores all printout methods for project 0


(module debug mzscheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    GENERAL   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "./data.scm" "data.scm")
  
; printline o -> void
; prints given line
(define (printline data)
  (begin (if data (print data))
         (printf "\n")))

; printline str -> void
; prints given line
(define (printfline data)
  (begin (if data (printf data))
         (printf "\n")))
  
; posn->str posn -> str
; converts posn to string
(define (posn->str p)
  (string-append (string-append "(" (string-append (number->string (posn.x p)) (string-append "," (number->string(posn.y p))))) ")"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide printline printfline posn->str))