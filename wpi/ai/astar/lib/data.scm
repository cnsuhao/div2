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
                                   (loop (+ i 1) (cdr props)))))
       
       ;; create copy function
       (eval 
        `(define ,(string->symbol (string-append name-s "-copy"))
           (lambda (v) 
             (list->vector (vector->list v)))))))))

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

(define (posn- p1 p2)
  (make-posn (- (posn.x p1) (posn.x p2))
             (- (posn.y p1) (posn.y p2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    MISC     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; range num num ->list
;; returns a list between low and high (high not included)
(define (range lo hi)
  (if (= lo hi) ()
      (cons lo (range (+ 1 lo) hi))))

; shuffle-list list num -> list
; a method to shuffle a list c times
(define (shuffle-list l c)
     (if (zero? c) l
	 (let-values ([(a b)
		       (let ([half (floor (/ (length l) 2))])
			 (values
			  (let loop ([l l][n half])
			    (if (zero? n) null
				(cons (car l) (loop (cdr l) (sub1 n)))))
			  (list-tail l half)))])
	   (shuffle-list
	    (let loop ([a a][b b][l null])
	      (cond
	       [(null? a) (append (reverse b) l)]
	       [(null? b) (append (reverse a) l)]
	       [(zero? (random 2))
		(loop (cdr a) b (cons (car a) l))]
	       [else
		(loop a (cdr b) (cons (car b) l))]))
	    (sub1 c)))))