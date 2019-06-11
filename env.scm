(load "symb-table.scm")

(define (make-env)
       (make-env/table-list '()))

(define (make-env/table-list table-list)
  table-list)

(define (env-symb-table-list env)
  env)

(define (env-symb-ref env symb)
  (letrec [(table-list 
	     (env-symb-table-list env))
	   (table-list-ref
	     (lambda (table-list)
	       (if (null? table-list)
		   '()
		   (let* [(table (car table-list))
			  (remain-table (cdr table-list))
			  (e (symb-table-ref table symb))]
		     (if (not (null? e))
			 e
			 (table-list-ref remain-table))))))]
    (table-list-ref table-list)))

(define (enter-scope env)
  (let [(table-list (env-symb-table-list env))
	(symb-table (make-symb-table))]
    (make-env/table-list (cons symb-table table-list))))

(define (leave-scope env)
  (let [(table-list (env-symb-table-list env))]
    (make-env/table-list (cdr table-list))))

(define (encounter-symb env symb type)
  (let* [(table-list (env-symb-table-list env))
	 (current-table (car table-list))
	 (remain-table-list (cdr table-list))
	 (new-entry (make-entry symb type))]
    (make-env/table-list (cons (add-entry current-table new-entry) remain-table-list))))

(define (env-symb-exist? env symb)
  (let [(ref (env-symb-ref env symb))]
    (not (null? ref))))

(define (symb-type-eq? env symb type)
  (let [(ref-type (env-symb-ref env symb))]
    (eq? ref-type type)))
