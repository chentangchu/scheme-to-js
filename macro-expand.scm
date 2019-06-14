(load "lib.scm")
(load "env.scm")

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

(define (match-rule? _ var-list expr-list)
  (if (null? var-list)
      (null? expr-list)
      (let [(var (car var-list))
	    (remain-var (cdr var-list))]
	(cond [(eq? var '...)
	       (if (null? (cdr remain-var))
		   (list (car remain-var) expr-list)
		   ((set! var (car remain-var))
		    (set! remain-var (cdr remain-var))
		    (match-rule _ 
				(reverse (append (list var '...) remain-var))
				(reverse expr-list))))]
	      [else
		(let [(expr (car expr-list))
		      (remain-expr (cdr expr-list))]
		  (cons (list var expr) (match-rule _ remain-var remain-expr)))]))))

(define (match-rule rule expr)
  (cond [(null? rule) '()]
	[(symbol? rule) (list rule expr)]
	[else (let [(sub-rule (car rule))
		    (remain-rule (cdr rule))]
		(cond [(eq? sub-rule '...)
		       (if (null? (cdr remain-rule))
			   (match-rule (car remain-rule) expr)
			   (begin (set! sub-rule (car remain-rule))
				  (set! remain-rule (cdr remain-rule))
				  (match-rule (reverse (append (list sub-rule '...) remain-rule))
					      (reverse expr))))]
		      [(eq? sub-rule '_)
		       (match-rule )]
		      [else (let [(sub-expr (car expr))
				(remain-expr (cdr expr))]
			    (append (match-rule sub-rule sub-expr)
				    (match-rule remain-rule remain-expr)))]))]))

(define (rule->expr-cond rule)
  (letrec* [(ellipsis-n 0)
	 (underscore-n 0)
	 (var-n 0)
	 (scan-rule (lambda (elem)
		      (cond [(eq? elem '...) 
			     (set! ellipsis-n (+ ellipsis-n 1))]
			    [(eq? elem '_)
			     (set! underscore-n (+ underscore-n 1))]
			    [else 
			      (set! var-n (+ 1 var-n))])))
	 (underscore-filter (lambda (var-expr-list)
			      (if (null? var-expr-list) #t
				  (let* [(var-expr (car var-expr-list))
					 (var (list-ref var-expr 0))
					 (expr (list-ref var-expr 1))]
				    (and (cond [(eq? var '_) (eq? var _)]
					       [else #t])
					 (underscore-filter (cdr var-expr-list))))))))]
    (map scan-rule rule)
    (lambda (_ expr)
      (and (> underscore-n 0)
	   (cond [(= ellipsis-n 0) #t]
		 [(= ellipsis-n 1) (> var-n 0)]
		 [else #f])
	   (>= (length expr) (- (+ underscore-n var-n) ellipsis-n))
	   (underscore-filter (match/rule-syntax expr rule)))))

(define (syntax-rule-expand expr)
  (let* [(rand (cdr expr))
	 (rule-list (cdr rand))
	 (rule->cond lambda ())]
    ()))

(define (reduce/list l last reduce/null reduce/elem concat)
  (let [(helper 
	  (lambda (l last)
	    (reduce/list l last reduce/null reduce/elem concat)))]
    (if (null? list)
	reduce/null
	(concat (reduce/elem last (car l))
		(helper (cdr l) (car l))))))

(define (pass/valid-rule-head?/valid-ellipsis? rule-head)
  (cond [(list? rule-head)
	  (if (null? rule-head) #f
	      (let* [(reduce/elem (lambda (last elem)
				   (cond [(eq? elem '...) (symbol? last)]
					 [(symbol? elem) #t]
					 [else (pass/valid-rule-head?/valid-ellipsis elem)])))]
		(reduce/list rule-head #t reduce/elem and)))]
	[else #f]))

(define (pass/valid-rule-head?/valid-var? rule-head)
  (cond [(list? rule-head)
	  (if (null? rule-head) #f
	      (let* [(reduce/elem (lambda (last elem)
				   (cond [(eq? elem '...) #t]
					 [(symbol? elem)
					  (cond [(env-symb-exist? env elem) #f]
						[else
						  (set! env (encounter-symb env elem 'symbol))])]
					 [else (pass/valid-rule-head?/valid-var? elem)])))]
		(reduce/list rule-head #t reduce/elem and)))]
	[else #f]))

(define (pass/valid-rule-head? rule-head)
  (let [(env (make-env))]
    (enter-scope env)
    (and (pass/valid-rule-head?/valid-ellipsis? rule-head)
	 (pass/valid-rule-head?/valid-var? rule-head))))

(define ellipsis-env (make-env))
(set! ellipsis-env (enter-scope ellipsis-env))

(define (pass/rule-head\ellipsis rule-head)
  (let [(reduce/elem (lambda (last elem)
		       (cond [(eq? elem '...) '()]
			     [(symbol? elem) elem]
			     [else
			       (pass/rule-head\ellipsis elem)])))
	(collect-var (lambda (a b) 
		       (if (null? a) b (cons a b))))]
    (reduce/list rule-head '() reduce/elem collect-var)))

(define (rule-head-env rule-head env)
  (let [(reduce/elem (lambda (last elem)
		       (cond [(eq? elem '...)
			      (set! env (encounter-symb env last 'ellipsis)) #t]
			     [(symbol? elem)
			      (set! env (encounter-symb env last 'identifier)) #t]
			     [else
			       (set! env (pass/rule-head-env elem env))])) #t)]
    (reduce/list rule-head '() reduce/elem or) env))

(define (expr-match-rule? rule-head expr env)
  (cond [(null? rule-head) (null? expr)]
	[(pair? rule-head)
	 (and (list? expr)
	      (let [(rule-head-elem (car rule-head))
		    (rule-head-remain (cdr rule-head))]
		(cond [(symbol? rule-head-elem)
		       (cond [(symb-type-eq? env rule-head-elem 'ellipsis)
			      ((if (null? rule-head-remain) #t
				   (expr-match-rule? (reverse rule-head) (reverse expr) env)))]
			     [else
			       (expr-match-rule? (cdr rule-head) (cdr expr) env)])]
		      [else 
			(and
			  (pair? expr)
			  (expr-match-rule? rule-head-elem (car expr))
			  (expr-match-rule? rule-head-remain (cdr expr)))])))]
	[else #f]))

(define (rule-head->premise rule-head)
  (let [(rule-head-env (rule-head-env rule-head (make-env)))]
    (lambda (expr) (expr-match-rule? rule-head expr rule-head-env))))

(define (rule-head->template rule-body env)
  (let [(rename-expr (rule-body-rename rule-body env))]
    ()))

(define (syntax-rule-expand rule)
  (let [(rule-head (car rule))
	(rule-body (cadr rule))]
    (list (rule-head->premise rule-head)
	  (rule-body->template rule-body))))

(define syntax-rule-env (make-env))

(define (valid-rule? rule)
  (cond [(and (list? rule)
	      (= (length rule) 2))
	 (let [(rule-head (car rule))
	       (rule-body (cadr rule))]
	   (and (valid-rule-head? rule-head)
		(begin (set!  syntax-rule-env
			 (rule-head->env rule-head syntax-rule-env)) #t)
		(valid-rule-body? rule-body)))]
	[else #f]))

(define (valid-syntax-rules? rule-list)
  (fold (map valid-rule? rule-list) #t and))

(define (syntax-rules->expand-lambda rule-list)
  (let [(premise-expand-list (map rule->premise-expand rule-list))]
    (lambda (expr)
      (let [(loop-premise-expand
	      (lambda (premise-expand-list)
		(if (null? premise-expand-list) '()
		    (let* [(premise-expand (car premise-expand-list))
			   (premise (car premise-expand))
			   (expand (cadr premise-expand))]
		      (cond [(premise expr) (expand expr)]
			    [else (loop-premise-expand (cdr premise-expand-list))])))))]
	(loop-premise-expand premise-expand-list)))))
