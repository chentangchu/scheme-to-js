(define (macro-expand expr)
  (if (pair? expr)
      (cond [(define-macro? expr) (define-macro-expand expr)]
	    [(let-macro? expr) (let-macro-expand expr)]
	    [else (map macro-expand expr)])
      expr))

(define (let-macro? expr)
  (letrec [(op (car expr))
	   (rand (cdr expr))
	   (var-value-list?  (lambda (vv-list)
			       (if (null? vv-list) #t
				   (let [(vv (car vv-list))
					 (remain-list (cdr vv-list))]
				     (cond [(and (list? vv) (= (length vv) 2))
					    (var-value-list? remain-list)]
					   [else #f])))))]
    (and (eq? op 'let)
	 (>= (length rand) 2)
	 (list? (list-ref rand 0))
	 (var-value-list? (list-ref rand 0)))))

(define (let-macro-expand expr)
  (let* [(rand (cdr expr))
	 (var-value-list (car rand))
	 (let-body (cdr rand))
	 (var-list (map car var-value-list))
	 (value-list (map cadr var-value-list))
	 (exp-let-body (map macro-expand let-body))
	 (exp-value-list (map macro-expand value-list))
	 (lambda-op (append (list 'lambda var-list) exp-let-body))]
    (cons lambda-op exp-value-list)))

(define (define-macro? expr)
  (let [(op (car expr))
	(rand (cdr expr))]
    (and (eq? op 'define)
	 (>= (length rand) 2)
	 (pair? (list-ref rand 0)))))

(define (define-macro-expand expr)
  (let* [(rand (cdr expr))
	 (define-head (list-ref rand 0))
	 (define-body (cdr rand))
	 (lambda-name (car define-head))
	 (arg-list (cdr define-head))
	 (exp-arg-list (map macro-expand arg-list))
	 (exp-define-body (map macro-expand define-body))
	 (lambda-body (append (list 'lambda exp-arg-list) exp-define-body))]
    (list 'define lambda-name lambda-body)))
