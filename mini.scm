; A quick macro so we don't need to quote the body
(load "lib.scm")
(load "env.scm")

(define env (make-env))

(define-syntax scheme->js
    (syntax-rules ()
        [(_ expr)
         (scheme->js* (quote expr))]))

(define (scheme->js* expr)
  (cond [(define-expr? expr) (define-expr->js expr)]
	[(eval-expr? expr) (eval-expr->js expr)]
	[else (error "Undefined expression type:" expr)]))

(define (emit-code js-code)
  (display js-code))

(define (define-expr? expr)
  (and (list? expr)
       (= (length expr) 3)
       (eq? 'define (list-ref expr 0))
       (symbol?     (list-ref expr 1))
       (eval-expr?  (list-ref expr 2))))

(define (expr-type expr)
  (cond [(lambda-expr? expr) 'lambda]
	[else 'other]))

(define (define-expr->js expr)
  (cond [(not (define-expr? expr))
	 (error "Undefined expression type:" expr)]
    	[else
	  (set! env (encounter-symb env var (expr-type value)))
	  (let* [(var     (list-ref expr 1))
		 (value   (list-ref expr 2))]
	    (emit-code 
	      (string-append "let " (symbol->string var) " = "  (eval-expr->js value))))]))

(define (eval-expr? expr)
  (or (primitive-expr? expr)
      (apply-expr? expr)
      (lambda-expr? expr)))

(define (eval-expr->js expr) 
  (cond [(not (eval-expr? expr)) 
	 (error "Undefined expression type:" expr)]
	[(primitive-expr? expr)  (primitive-expr->js expr)]
	[(lambda-expr? expr) (lambda-expr->js expr)]
	[(apply-expr? expr) (apply-expr->js expr)]))

(define (primitive-expr? expr)
  (not (list? expr)))

(define (primitive-expr->js expr)
  (cond [(not (primitive-expr? expr))
	 (error "Undefined expression type:" expr)]
	[(boolean? expr) (emit-code (if expr "true" "false"))]
        [(string? expr) (emit-code (string-append "\"" expr "\""))]
        [(number? expr) (emit-code (number->string expr))]
        [(symbol? expr) (emit-code (symbol->string expr))]))

(define (lambda-expr? expr)
  (and (list? expr)
       (> (length expr) 2)
       (eq? 'lambda (list-ref expr 0))
       (list?       (list-ref expr 1))
       (eval-expr?  (list-ref-last expr))))

(define (lambda-expr->js expr)
  (define (para-list->string para-list)
    (let [(para-string-list (map symbol->string para-list))]
      (string-join para-string-list ",")))
  (define (body-list->string body-list)
    (letrec [(body-segm-list (map scheme->js* body-list))
	  (body-segm->js (lambda (string-list)
			    (if (null? (cdr string-list))
				(string-append "return " (car string-list) ";")
				(string-append (car string-list) ";" 
					       (body-segm->js (cdr string-list))))))]
      (body-segm->js body-segm-list)))

  (set! env (enter-scope env))
  (let* [(para-list (cadr expr))
	 (body-list (cddr expr))
	 (js-code 
	   (string-append "(("   (para-list->string para-list) ")"
			  " => " (body-list->string body-list) ")"))]
    (set! env (leave-scope env))
    (emit-code js-code)))

(define (operator? expr)
  (cond [(symbol? expr) (and
			  (env-symb-exist? env expr)
			  (symb-type-eq? env expr 'lambda))]
	[else (lambda-expr? expr)]))

(define (apply-expr? expr)
  (and (list? expr)
       (> (length expr) 0)
       (operator? (list-ref expr 0))))

(define (apply-expr->js expr)
  (cond [(not (apply-expr? expr)) (error "Undefined expression type:" expr)]
	[(tagged-expr? '= expr) (js-infix "===" expr)]
        [(tagged-expr? '+ expr) (js-infix "+" expr)]
        [(tagged-expr? '- expr) (js-infix "-" expr)]
        [(tagged-expr? '* expr) (js-infix "*" expr)]
        [(tagged-expr? '/ expr) (js-infix "/" expr)]
        [(tagged-expr? 'null? expr) (null?-expr->js expr)]
        [(tagged-expr? 'get expr) (get-expr->js expr)]
        [(tagged-expr? 'car expr) (car-expr->js expr)]
        [(tagged-expr? 'cdr expr) (cdr-expr->js expr)]
	[else 
	  (let* [(op (car expr))
		 (rand (cdr expr))
		 (eval-op (eval-expr->js op))
		 (eval-rand (map eval-expr->js rand))]
	    (string-append eval-op "(" (string-join eval-rand ",") ")"))]))

(define (js-infix op expr)
    (string-append
        "("
        (scheme->js* (cadr expr))
        op
        (scheme->js* (caddr expr))
        ")"))

(define (null?-expr->js expr)
    (let [(ls (scheme->js* (cadr expr)))]
        (string-append "(0 === (" ls ").length)")))

(define (get-expr->js expr)
    (let [(ls (scheme->js* (cadr expr)))
          (idx (scheme->js* (caddr expr)))]
        (string-append "(" ls ")[" idx "]")))

(define (car-expr->js expr)
    (get-expr->js `(get ,(cadr expr) 0)))

(define (cdr-expr->js expr)
    (let [(ls (scheme->js* (cadr expr)))]
        (string-append "(" ls ").slice(1)")))
