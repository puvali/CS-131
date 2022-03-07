#lang racket
(provide (all-defined-out))

;; 1. Write a Scheme procedure (expr-compare x y)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Auxiliary functions   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this is from the TAs' hint code repo
(define (lambda? x)
  (member x '(lambda λ)))



;; x declares X and y declares Y --> bound variable X!Y
(define (map-bound-var x y xmap?)
  (cond

   ;; empty hash
   [(equal? x '())
    (hash)]

   ;; maps (car x) or (car y) to (car x)!(car y)
   [(not (equal? (car x) (car y)))
    (if xmap?
	(hash-set (map-bound-var (cdr x) (cdr y) xmap?)
		  (car x)
		  (string->symbol (string-append (symbol->string (car x))
						 "!"
						 (symbol->string (car y)))))
	(hash-set (map-bound-var (cdr x) (cdr y) xmap?)
		  (car y)
		  (string->symbol (string-append (symbol->string (car x))
						 "!"
						 (symbol->string (car y))))))]

   ;; X and Y are equal
   [else
    (map-bound-var (cdr x) (cdr y) xmap?)]
   ))



;; to get the mapped variable name
(define (mapped-id id maps)
  (cond

   [(equal? maps '())
    "no mapping"]

   ;; returns the key for id in (car maps)
   [(equal? (hash-ref (car maps) id "no mapping")
	    "no mapping")
    (mapped-id id (cdr maps))]

   [else
    (hash-ref (car maps) id)]
   ))
	    


;; like map-bound-var, except it maps to itself and adds to the existing hash table
(define (self-map l)
  (cond

   [(equal? l '())
    (hash)]

   [else
    (hash-set (self-map (cdr l))
	      (car l)
	      (car l))]
   ))	   



;; uses hash maps to set the new variable names in a given list l
(define (update-ids l maps hd?)
  (cond

   [(equal? l '())
    '()]

   ;; don't change identifier for quote
   [(equal? (car l) 'quote)
    l]

   ;; don't change boolean literals
   [(boolean? (car l))
    (cons (car l)
	  (update-ids (cdr l) maps #f))]

   ;; starts with if
   [(and hd?
	 (equal? (car l) 'if))
    (cons 'if
	  (update-ids (cdr l) maps #f))]

   ;; starts with lambda --> do updates for the rest of the list 
   [(and hd?
	 (lambda? (car l)))
    (cons (car l)
	  (cons (cadr l)
		(update-ids (cddr l)
			    (cons (self-map (cadr l))
				  maps)
			    #f)))]

   ;; list
   [(list? (car l))
    (cons (update-ids (car l)
		      maps
		      #t)
	  (update-ids (cdr l)
		      maps
		      #f))]

   ;; look for a mapping and update accordingly
   [else
    (cons (if (equal? (mapped-id (car l) maps)
		      "no mapping")
	      (car l)
	      (mapped-id (car l) maps))
	  (update-ids (cdr l)
		      maps
		      #f))]
   ))
     
;; end auxiliaries



(define (expr-compare x y)
  (cond

   ;; no difference - default to x
   [(equal? x y)
    x]

   ;; boolean literals
   [(and (boolean? x)
	 (boolean? y))
    (if x '% '(not %))]

   ;; one is not a list OR both are lists of unequal length
   [(or
     (or (not (list? x))
	 (not (list? y)))
     (and (list? x)
	  (list? y)
	  (not (equal? (length x) (length y)))))
    (list 'if '% x y)]

   ;; lists of equal length
   [else
    (compare-lists x y)]
   ))



;; for lists of equal length
(define (compare-lists x y)
  (cond

   ;; both are if expressions
   [(and (equal? (car x) 'if)
	 (equal? (car y) 'if))
    (compare-nonlambdas x y)]

   ;; either is if expr OR either is quote
   [(or
     (or (equal? (car x) 'if)
	 (equal? (car y) 'if))
     (or (equal? (car x) 'quote)
	 (equal? (car y) 'quote)))
    (list 'if '% x y)]

   ;; both use the same lambda 
   [(and (lambda? (car x))
	 (equal? (car x) (car y)))
    (cond

     ;; unequal number of lambda args
     [(not (equal? (length (cadr x)) (length (cadr y))))
      (list 'if '% x y)]

     ;; equal number of lambda args
     [else
      (cond

       ;; LDA lambda 
       [(equal? (car x) 'lambda)
	(compare-lambdas (cdr x) (cdr y) 'lambda '() '())]

       ;; SVM lambda
       [(equal? (car x) 'λ)
	(compare-lambdas (cdr x) (cdr y) 'λ '() '())])])]

   ;; both use different lambdas
   [(and (lambda? (car x))
	 (lambda? (car y)))
    (cond

     ;; unequal # of args
     [(not (equal? (length (cadr x)) (length (cadr y))))
      (list 'if '% x y)]

     [else
      ;; use SVM lambda where they disagree
      (compare-lambdas (cdr x) (cdr y) 'λ '() '())])]

   ;; only one uses lambda
   [(or (lambda? (car x))
	(lambda? (car y)))
    (list 'if '% x y)]

   [else
    (compare-nonlambdas x y)]

   ))



;; equal length lists, not lambdas
(define (compare-nonlambdas x y)
  (cond

   ;; both empty
   [(and (equal? x '())
	 (equal? y '()))
    '()]

   ;; default to x when there is a mismatch
   [(equal? (car x) (car y))
    (cons (car x) (compare-nonlambdas (cdr x) (cdr y)))]

   [(and (boolean? (car x))
	 (boolean? (car y)))
    (cons (if (car x) '% '(not %))
	  (compare-nonlambdas (cdr x) (cdr y)))]

   [else
    (cond

     ;; start with lists of same length
     [(and (list? (car x))
	   (list? (car y))
	   (equal? (length (car x)) (length (car y))))
      (cons (compare-lists (car x) (car y))
	    (compare-nonlambdas (cdr x) (cdr y)))]

     [else
      (cons (list 'if '% (car x) (car y))
	    (compare-nonlambdas (cdr x) (cdr y)))])]
   ))
       



;; lambda fns with equal number of arguments
;; whichlambda is lambda or λ
(define (compare-lambdas x y whichlambda xmaps ymaps)
  (list whichlambda
	(lambda-formals (car x) (car y))
	(lambda-expr (cadr x)
		     (cadr y)
		     (cons (map-bound-var (car x) (car y) #t) xmaps)
		     (cons (map-bound-var (car x) (car y) #f) ymaps))
	))



;; for the arguments
(define (lambda-formals x y)
  (cond

   [(and (equal? x '())
	 (equal? y '()))
    '()]

   ;; same identifier name
   [(equal? (car x) (car y))
    (cons (car x)
	  (lambda-formals (cdr x) (cdr y)))]

   ;; different formal id --> use bound variable 
   [else
    (cons (string->symbol (string-append (symbol->string (car x))
					 "!"
					 (symbol->string (car y))))
	  (lambda-formals (cdr x) (cdr y)))]
   ))



;; fn body
(define (lambda-expr x y xmaps ymaps)
  (let
      ([xargid
	(if (equal? (mapped-id x xmaps) "no mapping")
	    x
	    (mapped-id x xmaps))]

       [yargid
	(if (equal? (mapped-id y ymaps) "no mapping")
	    y
	    (mapped-id y ymaps))])

    (cond
     [(equal? xargid yargid)
      xargid]

     ;; both lists 
     [(and (list? x)
	   (list? y))
      (expr-compare (update-ids x xmaps #t)
		    (update-ids y ymaps #t))]

     [(list? x)
      (expr-compare (update-ids x xmaps #t) xargid)]

     [(list? y)
      (expr-compare x (update-ids y ymaps #t))]

     [else
      (expr-compare xargid yargid)]
     )))



;; 2. Write a Scheme procedure (test-expr-compare x y) that tests your implementation
;; of expr-compare

;; this is from the TAs' hint code repo
(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ((% #t)) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)) ,(expr-compare x y))))))



;; 3. Define two Scheme variables test-expr-x and test-expr-y that contain data that
;; can be interpreted as Scheme expressions that test expr-compare well

(define test-expr-x
  '(lambda (a b)
     (lambda (c)
       (if c
	   (quote b)
	   (lambda (z) (* a b z))))))

(define test-expr-y
  '(lambda (d e)
     (lambda (f)
       (if f
	   (quote e)
	   (lambda (w) (quote #t))))))
