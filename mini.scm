; A quick macro so we don't need to quote the body
(define-syntax scheme->js
    (syntax-rules ()
        [(_ expr)
         (scheme->js* (quote expr))]))

(define (scheme->js* expr)
  (cond [(define-expr? expr) (define-expr->js expr)]
	[(eval-expr? expr) (eval-expr->js expr)]
	[else (error "Undefined expression type:" expr)]))

(define (string-join strs joiner)
    (define (helper strs acc)
        (if (null? strs)
            acc
            (helper (cdr strs)
                    (string-append
                        acc
                        joiner
                        (car strs)))))
    (if (null? strs)
        ""
        (helper (cdr strs) (car strs))))

(define (list-last-elem l)
  (if (= (length l) 1) 
      (car l) 
      (list-last-elem (cdr l))))

(define (tagged-expr? tag expr)
    (and (pair? expr)
         (eq? (car expr) tag)))

(define (define-expr? expr)
  (and (tagged-expr? 'define expr) 
       (= (length expr) 3) 
       (symbol? (cadr expr))
       (eval-expr? (caddr expr))))

(define (define-expr->js expr)
  (cond [(not (define-expr? expr))
	 (error "Undefined expression type:" expr)]
    	[else
	  (let* [(operand (cdr expr))
		 (var     (car operand))
		 (value   (cadr operand))]
	   (string-append "let " (symbol->string var) " = " (eval-expr->js value)))]))

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
  (not (pair? expr)))

(define (primitive-expr->js expr)
  (cond [(not (primitive-expr? expr))
	 (error "Undefined expression type:" expr)]
	[(boolean? expr) (if expr "true" "false")]
        [(string? expr) (string-append "\"" expr "\"")]
        [(number? expr) (number->string expr)]
        [(symbol? expr) (symbol->string expr)]))

(define (lambda-expr? expr)
  (and (tagged-expr? 'lambda expr) 
       (> (length expr) 2) 
       (list? (cadr expr))
       (eval-expr? (list-last-elem (cddr expr)))))

(define (lambda-expr->js expr)
  (define (para-list->string para-list)
    (let [(para-string-list (map symbol->string para-list))]
      (string-join para-string-list ",")))
  (define (body-list->string body-list)
    (letrec [(body-segm-list (map scheme->js* body-list))
	  (body-segm->js (lambda (string-list) 
			    (if (= 1 (length string-list))
				(string-append "return " (car string-list) ";")
				(string-append (car string-list) ";" 
					       (body-segm->js (cdr string-list))))))]
      (body-segm->js body-segm-list)))
  (let* [(para-list (cadr expr))
	 (body-list (cddr expr))]
    (string-append
            "((" (para-list->string para-list) ") => " (body-list->string body-list) ")")))

(define (operator? expr)
  (or (lambda-expr? expr)
      (symbol? expr)))

(define (apply-expr? expr)
  (and (list? expr) 
       (> (length expr) 0)
       (operator? (car expr))
       (not (tagged-expr? 'define expr))
       (not (tagged-expr? 'lambda expr))))

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
