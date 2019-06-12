; A quick macro so we don't need to quote the body
(load "lib.scm")
(load "env.scm")

(define env (make-env))

(set! env (enter-scope env))
(set! env (encounter-symb env '= 'lambda))
(set! env (encounter-symb env '+ 'lambda))
(set! env (encounter-symb env '- 'lambda))
(set! env (encounter-symb env '* 'lambda))
(set! env (encounter-symb env '/ 'lambda))
(set! env (encounter-symb env 'null? 'lambda))
(set! env (encounter-symb env 'get 'lambda))
(set! env (encounter-symb env 'cons 'lambda))
(set! env (encounter-symb env 'car 'lambda))
(set! env (encounter-symb env 'cdr 'lambda))
(set! env (encounter-symb env 'display 'lambda))

(define-syntax scheme->js
    (syntax-rules ()
        [(_ expr)
         (emit-code (scheme->js* (quote expr)))]))

(define (scheme->js* expr)
  (cond [(define-expr? expr) (define-expr->js expr)]
	[(eval-expr? expr) (eval-expr->js expr)]
	[else (error #f "error with scheme->js*:" expr)]))

(define (emit-code js-code)
  (display js-code))

(define (define-expr? expr)
;  (display "define-expr? => ") (display expr) (display "\n" )
  (and (list? expr)
       (= (length expr) 3)
       (eq? 'define (list-ref expr 0))
       (symbol?     (list-ref expr 1))
       (eval-expr?  (list-ref expr 2))))

(define (expr-type expr)
  (cond [(lambda-expr? expr) 'lambda]
	[else 'other]))

(define (define-expr->js expr)
;  (display "define-expr->js ==> ") (display expr) (display "\n")
  (cond [(not (define-expr? expr))
	 (error #f "error with define->js" expr)]
    	[else
	  (let* [(var     (list-ref expr 1))
		 (value   (list-ref expr 2))]
	    (set! env (encounter-symb env var (expr-type value)))
	    (string-append "let " (symbol->string var) " = "  (eval-expr->js value)))]))

(define (eval-expr? expr)
;  (display "eval-expr? =>")(display expr) (display "\n")
  (or (primitive-expr? expr)
      (apply-expr? expr)
      (lambda-expr? expr)))

(define (eval-expr->js expr) 
;  (display "eval-expr->js ==> ") (display expr) (display "\n")
  (cond [(not (eval-expr? expr)) 
	 (error #f "error with eval-expr->js:" expr)]
	[(primitive-expr? expr)  (primitive-expr->js expr)]
	[(lambda-expr? expr) (lambda-expr->js expr)]
	[(apply-expr? expr) (apply-expr->js expr)]))

(define (primitive-expr? expr)
  (not (list? expr)))

(define (primitive-expr->js expr)
  (cond [(not (primitive-expr? expr))
	 (error #f "error with primitive-expr->js:" expr)]
	[(boolean? expr) (if expr "true" "false")]
        [(string? expr) (string-append "\"" expr "\"")]
        [(number? expr) (number->string expr)]
        [(symbol? expr) (symbol->string expr)]))

(define (lambda-expr? expr)
;  (display "lambda-expr? ==> ") (display expr) (display "\n")
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
				  ;(string-append "return " (car string-list) ";")
				  (string-append "return " (car string-list))
				  (string-append (car string-list) ";"
						 (body-segm->js (cdr string-list))))))]
;      (display "==>")(write body-list)(display "\n")
;      (display "==>")(display body-segm-list)(display "\n")
      (body-segm->js body-segm-list)))
;  (display "lambda-expr ==> ") (display expr) (display "\n")
  (set! env (enter-scope env))
  (let* [(para-list (list-ref expr 1))
	 (body-list (cddr expr))]
    (set! env (leave-scope env))
    (string-append "(("   (para-list->string para-list) ")"
		   " => {" (body-list->string body-list) "})")))

(define (operator? expr)
;  (display "operator? ==> ") (display expr) (display "\n")
  (cond [(symbol? expr)
	 #t]
;	 (and (env-symb-exist? env expr)
;	      (symb-type-eq? env expr 'lambda))]
	[else (lambda-expr? expr)]))

(define (apply-expr? expr)
;  (display "apply-expr? ==> ") (display expr ) (display "\n")
  (and (list? expr)
       (> (length expr) 0)
       (operator? (list-ref expr 0))))

;(define (math-expr? expr)
;  (let [(op (list-ref expr 0))
;	(rand (cdr expr))]
;    (or (eq? op '+)
;	(eq? op '-)
;	(eq? op '*)
;	(and (eq? op '/)
;	     (> (length ) 0)))))

(define (apply-expr->js expr)
  ;(display "apply-expr->js ==>") (display expr) (display "\n")
  (if (apply-expr? expr)
      (let [(op (list-ref expr 0))
	    (rand (cdr expr))]
	(cond [(tagged-expr? '= expr) (js-infix "===" expr)]
	      [(tagged-expr? '+ expr) (js-infix "+" expr)]
	      [(tagged-expr? '- expr) (js-infix "-" expr)]
	      [(tagged-expr? '* expr) (js-infix "*" expr)]
	      [(tagged-expr? '/ expr) (js-infix "/" expr)]
	      [(tagged-expr? 'null? expr) (null?-expr->js expr)]
	      [(tagged-expr? 'get expr) (get-expr->js expr)]
	      [(tagged-expr? 'car expr) (car-expr->js expr)]
	      [(tagged-expr? 'cdr expr) (cdr-expr->js expr)]

	      [(eq? op 'list)
	       (apply-expr->js (cons 'Array rand))]

	      [(eq? op 'display)
	       (apply-expr->js (cons 'console.log rand))]

	      [(eq? op 'cons) 
	       (if (= (length rand) 2)
		   (apply-expr->js (cons 'Array rand))
		   (error #f "too many arguments with operator cons"))]
	      [else
		(let* [(op (car expr))
		       (rand (cdr expr))
		       (eval-op (eval-expr->js op))
		       (eval-rand (map eval-expr->js rand))]
		  (string-append eval-op "(" (string-join eval-rand ",") ")"))]))
      (error #f "error with apply-expr->js" expr)))

(define (js-infix op expr)
    (string-append
        "("
        (scheme->js* (cadr expr))
        op
        (scheme->js* (caddr expr))
        ")"))

(define (null?-expr->js expr)
    (let [(ls (scheme->js* (cadr expr)))]
        (string-append "Array.isArray(" ls ") && (0 === (" ls ").length)")))

(define (get-expr->js expr)
    (let [(ls (scheme->js* (cadr expr)))
          (idx (scheme->js* (caddr expr)))]
        (string-append "(" ls ")[" idx "]")))

(define (car-expr->js expr)
    (get-expr->js `(get ,(cadr expr) 0)))

(define (cdr-expr->js expr)
    (let [(ls (scheme->js* (cadr expr)))]
        (string-append "(" ls ").slice(1)")))
