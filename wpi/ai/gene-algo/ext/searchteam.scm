(include "../lib/debug.scm")

(define sprinter 0)
(define walker   1)
(define crawler  2)
(define sitter   3)

(define distance 0)
(define sight    1)
(define hearing  2)

;; storage of variables for teams, manually created
(define attributes
  (vector (vector 0 0 0) (vector 0 0 0) 
          (vector 0 0 0) (vector 0 0 0)))

;; retrive attribute for a type
(define (get-attribute type attr)
  (vector-ref (vector-ref attributes type) attr))

;; set attribute for a type
(define (set-attribute type attr value)
  (vector-set! (vector-ref attributes type) attr value))

;; copy a team
(define (copy-team team)
  (list->vector (vector->list team)))

;; check if teams are the same
(define (team-eqv? t1 t2)
  (let loop ((l1 (vector->list t1))
             (l2 (vector->list t2)))
    (if (or (null? l1)) #t
        (if (eqv? (car l1) (car l2))
            (loop (cdr l1) (cdr l2))
            #f))))

;; distance is calculated by multiplying the sum of difference of all the types times the difference in number of types + 1
(define (distance-teams t1 t2)
  (let  ((types1 (foldr (lambda(a b) (if (zero? a) b (+ b 1))) 0 (vector->list t1)))
         (types2 (foldr (lambda(a b) (if (zero? a) b (+ b 1))) 0 (vector->list t2))))
    (* (+ (abs (- (vector-ref t1 0) (vector-ref t2 0)))
          (abs (- (vector-ref t1 1) (vector-ref t2 1)))
          (abs (- (vector-ref t1 2) (vector-ref t2 2)))
          (abs (- (vector-ref t1 3) (vector-ref t2 3))))
       (+ 1 (abs (- types1 types2))))))

;; print teams
(define (print-teams tt)
  (let loop ((tt tt) (i 0))
    (if (null? tt) (debug-print "~n")
        (begin
          (debug-print "Team~a: " i)
          (print-team (car tt))
          (debug-print "~n")
          (loop (cdr tt) (+ i 1))))))


;; print a team
(define (print-team t)
  (debug-print "~a " (vector-ref t 0))
  (debug-print "~a " (vector-ref t 1))
  (debug-print "~a " (vector-ref t 2))
  (debug-print "~a"  (vector-ref t 3)))