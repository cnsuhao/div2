(include "./data.scm")
(include "./debug.scm")

; the working memory, things are automatically added
(define facts null)
(define rules null)

; initialized at the start of run so "reset" will work
(define initfacts null)
(define initrules null)

; States for the program (debug prints debug commands
(define running #f)

; commonly used constants
(define true 'yes)
(define false 'no)
(define nil #f)


; set "running" to false
(define (stop)
  (set! running #f))


(define (initialize)
  (set! initfacts facts)
  (set! initrules rules))

(define (reset)
  (set! facts initfacts)
  (set! rules initrules))

(define (reset-facts)
  (set! facts initfacts))

(define (reset-rules)
  (set! rules initrules))



(define (clear)
  (set! facts null)
  (set! rules null)
  (set! initfacts null)
  (set! initrules null))

(define (clear-facts)
  (set! facts null)
  (set! initfacts null))

(define (clear-rules)
  (set! rules null)
  (set! initrules null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   FACTS   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; function to create facts
(define-syntax deffacts
  (syntax-rules()
    ((deffacts (n is v) ...)
     (begin (define-fact n is v) ...))))


;  define-fact wrapper
(define-syntax assert
  (syntax-rules()
    ((assert n is v)
     (define-fact n is v))))

;  define-fact wrapper
(define-syntax modify
  (syntax-rules()
    ((assert n is v)
     (define-fact n is v))))

; helper function to create facts
(define-syntax define-fact
  (syntax-rules()
    ((define-fact n is v)
     (begin (retract n) (set! facts (append facts (list (cons n v))))))))

; remove fact from list
(define (retract name)
  (set! facts (filter (lambda (f) (not (eqv? (car f) name))) facts)))

; check whether or not there is a fact which e
(define (fact-exists? name)
  (let loop ((ff facts))
    (if (null? ff) #f
        (if (eqv? (car (car ff)) name) #t
            (loop (cdr ff))))))

; returns the value stored in given fact
(define (fact-value name)
  (let loop ((ff facts) (i 0))
    (if (null? ff) #f
        (if (eqv? (car (car ff)) name) (cdr (car ff))
            (loop (cdr ff) (+ i 1))))))

(assert '_ is 'DNE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   CONDITIONS   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Reformat passed condition into a lambda
(define-syntax process-cond
  (syntax-rules(exists has is same-as same-name can bind and or xor not )
    ((_ (has n) args)
     `(lambda ,args (fact-exists? ,n)))
    ((_ (n exists) args)
     `(lambda ,args(fact-exists? ,n)))
    
    ((_ (is n) args)
     `(lambda ,args (eqv? 'yes (fact-value ,n))))
    ((_ (can n) args)
     `(lambda ,args (eqv? 'yes (fact-value ,n))))
    
    ((_ (n is v) args)
     `(lambda ,args (eqv? ,v (fact-value ,n))))
    ((_ (n1 same-as n2) args)
     `(lambda ,args (eqv? (fact-value ,n1) (fact-value ,n2))))
    ((_ (n1 same-name n2) args)
     `(lambda ,args (eqv? ,n1 ,n2)))


    ((_ (and c ...) args)
     `(lambda ,args (and ,@(append (cddr (process-cond c ())) ...))))
    ((_ (and c ...) args)
     `(lambda ,args (or ,@(append (cddr (process-cond c ())) ...))))
    
    ((_ (c1 and c2) args)
     `(lambda ,args (and ,@(append (cddr (process-cond c1 ())) (cddr (process-cond c2 ()))))))
    ((_ (c1 or c2) args)
     `(lambda ,args (or ,@(append (cddr (process-cond c1 ())) (cddr (process-cond c2 ()))))))
    ((_ (c1 xor c2) args)
     `(lambda ,args (not (eqv? ,@(append (cddr (process-cond c1 ())) (cddr (process-cond c2 ())))))))
    ((_ (not c) args)
     `(lambda ,args (not ,@(cddr (process-cond c ())))))
    
    ((_ (n is op v) args)
     `(lambda ,args (op (fact-value ,n) ,v )))
    ((_ (n op v) args)
     `(lambda ,args (op ,n ,v)))
    ))


; figure out how specific the passed condition is
(define-syntax analyze-cond
  (syntax-rules(exists has is and or xor not same-as can same-name)
    ; seeing if there is a fact with the name is worth 1
    ((_ (has n)) 1)
    ((_ (n exists)) 1)
    
    ; checking for boolean is worth 2
    ((_ (is n))  2)
    ((_ (can n)) 2)
    
    ; checking against values is worth 3
    ((_ (name is value)) 3)
    ((_ (n1 same-as n2)) 3)
    ((_ (n1 same-name n2)) 3)
    
    ; ands/ors joints are worth all
    ((_ (and c ...))
     (+ (analyze-cond c) ... ))
    ((_ (or c ...))
     (+ (analyze-cond c) ... ))
    
    ((_ (c1 and c2))
     (+ (analyze-cond c1) (analyze-cond c2)))
    ((_ (c1 or c2))
     (+ (analyze-cond c1) (analyze-cond c2)))
    ((_ (c1 xor c2))
     (+ (analyze-cond c1) (analyze-cond c2)))
    
    ; nots are worth their child cond
    ((_ (not c))
     (analyze-cond c))
    
    ; name ops are 3
    ((_ (name op value)) 3)
    ((_ (name is op value)) 3)
    
    ; a function is worth 2
    ((_ ((c ...))) 2)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   RULES   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; IF THEN syntax instead of defrule
(define-syntax rule
  (syntax-rules(IF THEN)
    ((rule name (IF cond THEN act ...))
     (defrule name cond => act ...))))


; adds rules to WM
(define-syntax defrule
  (syntax-rules(=> priority : )
    ((defrule name : cond => act ...)
     (defrule name (priority 0) : (bind ('_ ('_ is ''DNE))) cond => act ...))
    
    ((defrule name : bind cond => act ...)
     (defrule name (priority 0) : bind cond => act ...))
    
    ((defrule name (priority p) : cond => act ...)
     (defrule name (priority p) : (bind ('_ ('_ is ''DNE))) cond => act ...))
    
    ((defrule name (priority p) : bind cond => act ...)
     (begin (retract-rule name)
            (set! rules (append rules (list (list name
                                                  p 
                                                  (process-cond cond (map car bind)) 
                                                  (analyze-cond cond) 
                                                  `(lambda ,(map car bind) act ...)
                                                  bind))))))))



; bind sets variable to factname for all facts which evaluate to true for condition
; reformat bind command into a bindings list (equivilent of let*)
(define-syntax bind
  (syntax-rules()
    ((_ (var (c1 ...)) ...)
     (let ((total (list (cons var (process-cond (c1 ...) (list var))) ...)))
       ;loop through a list of binding data, previousvar is a list of required vars
       (let loop ((ll total)
                  (previousvar ()))
         (if (null? ll) ()
             ; set to new var and rebuild lambda with previous var
             (cons (cons (caar ll) (list (car (cdar ll)) (append previousvar (list (caar ll))) (caddr (cdar ll))))
                   (loop (cdr ll) (append previousvar (list (caar ll)))))))))))
                         



; remove rule from list
(define (retract-rule name)
  (set! rules (filter (lambda (r) (not (eqv? (car r) name))) rules)))


; get rule's priority
(define (get-priority rule)
  (cadr rule))

; check if cond evaluates to true
(define (check-cond rule)
  ((eval (caddr rule))))

; get get how specific cond is list from rule
(define (get-cond-detail rule)
  (car (cdddr rule)))

; get cond (printable) list from rule
(define (get-cond rule)
  (caddr rule))

; get actions list from rule
(define (get-actions rule)
  (cadr (cdddr rule)))

; get actions list from rule
(define (get-bindings rule)
  (caddr (cdddr rule)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   DEBUG   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; print all facts
(define (print-facts ff)
  (let loop ((ff ff))
    (if (not (null? ff))
        (begin (debug-print "    ~a is ~a~n" (caar ff) (cdar ff))
               (loop (cdr ff)))))
  (debug-print "~n"))

; print given rules
(define (print-rules rr)
  (if (not (null? rr))
      (begin (print-rule (car rr)) (print-rules (cdr rr)))))

; print given rules
(define (print-bindings bb)
  (let loop ((bb bb))
    (if (not (null? bb))
        (begin (debug-print "    ~a when ~a~n" (caar bb) (car (cdddar bb)))
               (loop (cdr bb)))))
  (debug-print "~n"))

; print a specific rule
(define (print-rule rule)
  (debug-print "  Rule Name: ~a~n" (car rule))
  (debug-print "  Priority: ~a~n" (get-priority rule))
  (debug-print "  Cond Complexity: ~a~n" (get-cond-detail rule))
  (debug-print "  Bindings:~n")
  (print-bindings (get-bindings rule))
  (debug-print "  Cond: ~n     ~a~n" (caddr (get-cond rule)))
  (debug-print "  Actions: ~n")
  (let loop ((acts (cddr (get-actions rule))))
    (if (not (null? acts))
        (begin (debug-print "     ~a~n" (car acts))
               (loop (cdr acts)))))
  (debug-print "~n"))

; prints the variables bound and what they are bound to
(define (print-bound dd)
  (if (not (null? (get-bindings (car dd))))
      (let* ((bb (map car (get-bindings (car dd))))
             (dd (cdr dd)))
        (let loop ((bb bb) (dd dd))
          (if (null? bb) (debug-print "~n")
              (begin (debug-print "~a,~a  " (car bb) (car dd))
                     (loop (cdr bb) (cdr dd))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   CHECK-RULE   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; fetch all rules which are triggered by current facts/bindings
; triggered-rules lof lor -> list (rule bindings)
(define (triggered-rules rr ff)
  (if (null? rr) ()
      (let ((bindings (check-bindings (get-bindings (car rr)) ff)))
        (append (check-rule-data (car rr) ff  bindings)
                (triggered-rules (cdr rr) ff)))))

; returns all bindings for which rule returns true
(define (check-rule-data r ff dd)
  (if (null? dd) ()
      (let ((checked (check-rule r ff (car dd))))
        (if (null? checked) (check-rule-data r ff (cdr dd))
            (cons (check-rule r ff (car dd))
                  (check-rule-data r ff (cdr dd)))))))

; check-rule determins if rule evaluates to true, if it does, it returns the rule and the given binding
(define (check-rule r ff data)
  (let ((cond (get-cond r)))
    (if (eval `((eval ,cond) ,@data))
        (cons r data)
        null)))
        



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   CHECK-BIND   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; go through each bind, return possible data binds
(define (check-bindings bb ff)
  (if (null? bb) ()
      (let ((initdata (check-binding (cdar bb) ff ())))
        (let loop ((bb (cdr bb)) (dd initdata))
          (if (null? bb) dd
            (let ((data (get-bound-facts (cdar bb) ff dd)))
                  (loop (cdr bb) data)))))))

; cycle through all data and facts with one bind rule
(define (get-bound-facts b ff dd)
  (if (null? dd) ()
      (append (check-binding b ff (car dd))
            (get-bound-facts b ff (cdr dd)))))
      
; cycle through all facts for constant bind and data
(define (check-binding b ff data)
  ; find facts which match new data
  (let ((newfacts (filter (lambda (f)
                           (eval `((eval ,b) ,@data ',f)))
                         (map car ff))))
    ; if no new data, return false
    (if (null? newfacts) ()
        ; otherwise reconstruct data list
        (let loop ((ff newfacts))
          (if (null? ff) ()
              (cons (append data (list (double-quote (car ff))))
                    (loop (cdr ff))))))))            


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   MAIN   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Function to execute the RI
(define (run)
  (begin
    (initialize)
    (set! running #t)   ;start
    
    ; print initial rules ane facts
    (debug-print "Facts: ~n")
    (print-facts facts)
    (debug-print "Rules: ~n")
    (print-rules rules)
    
    (let loop ()
      ; check if we need to stop
      (cond [(null? rules) (debug-print "----No more rules.  Stopping.~n")]
            [(not running) (debug-print "-----Stop command hit.  Stopping.~n")]
            [else 
             (let ((conflictset (triggered-rules rules facts)))
               ; display conflict set
               (debug-print "-----------------------------------~n")
               (debug-print "Triggered Rules: ~n")
               (map (lambda(r) 
                      (debug-print "  Rule: ~a~n" (caar r))
                      (debug-print "  Bound: ")
                      (print-bound r)
                      (debug-print "~n")) 
                    conflictset)
               
               (debug-print "~n")
               
               ;;;sort conflict set
               ; sort by priority
               (set! conflictset (sort conflictset (lambda (x y) (> (get-priority (car x)) (get-priority (car y))))))
               ; filter out all but first priority
               (set! conflictset (filter (lambda (x) (= (get-priority (car x)) (get-priority (caar conflictset)))) conflictset))
               ; sort remainder by detail
               (set! conflictset (sort conflictset (lambda (x y) (> (get-cond-detail (car x)) (get-cond-detail (car y))))))
               
               
               ; fire rule
               (if (null? conflictset) (begin (stop) (debug-print "----No more rules are triggered.  Stopping.~n"))
                   (let ((acts (get-actions (caar conflictset))))
                     (eval `((eval ,acts) ,@(cdar conflictset)))
                     (debug-print "----Rule ~a was fired~n----Bound:" (caaar conflictset))
                     (print-bound (car conflictset))))
               (loop))             
             ]))))
