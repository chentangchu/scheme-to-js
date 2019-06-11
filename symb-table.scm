(define (make-entry key value)
  (cons key value))

(define (entry-key entry)
  (car entry))

(define (entry-value entry)
  (cdr entry))

(define (make-symb-table)
  '())

(define (symb-table-ref symb-table symb)
  (if (null? symb-table)
      '()
      (let [(e (car symb-table))
	    (rest (cdr symb-table))]
	(if (eq? (entry-key e) symb)
	    e
	    (symb-table-ref rest symb)))))

(define (add-entry symb-table e)
  (cons e symb-table))

;(define (symb-table-join symb-table-list)
;  (let [(join-table (car symb-table-list))
;	(remain-list (cdr symb-table-list))]
;    (if (null? remain-list)
;	join-table
;	(let [(be-join-table (car remain-list))
;	      (be-join-table-remain (cdr remain-list))]
;	  (if (null? be-join-table)
;	      (symb-table-join (cons join-table be-join-table-remain))
;	      (let [(e (car be-join-table))
;		    (e-remain (cdr be-join-table))]
;		(symb-table-join 
;		  (cons (symb-table-add-entry join-table e) (cons e-remain be-join-table-remain)))))))))
