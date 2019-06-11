#!/usr/bin/scheme --script
(define-syntax bash-var?
  (syntax-rules ()
    [(_ s) 
     (let ([str (format "~a" 's)])
       (string=? "$" (substring str 0 1)))]))

(define-syntax eval-bash-var
  (syntax-rules ()
    [(_ ) ""]
    [(_ head tail ...)
     (cond 
	 [(bash-var? head)
	   (string-append (format " ~s" head) (eval-bash-var tail ...))]
	 [(string? 'head)
	   (string-append (format " ~s" 'head) (eval-bash-var tail ...))]
	 [else
	   (string-append (format " ~a" 'head) (eval-bash-var tail ...))])]))

(define-syntax bash 
  (syntax-rules ()
    [(_ command ...) (system (eval-bash-var command ...))])
)


(define-syntax show-bash 
  (syntax-rules ()
    [(_ command ...) (display (eval-bash-var command ...))])
)

(define arg-list (cdr (command-line)))
(define $filename (car arg-list))
(define $scm2js-filename (format "~a2js" $filename))

(bash touch $scm2js-filename)
(bash echo ";scheme to javascript" > $scm2js-filename)
;(bash echo "(load \"compiler.scm\")" >> $scm2js-filename)
(bash echo "(load \"mini.scm\")" >> $scm2js-filename)
;(bash echo "(display (scheme->js (begin " >> $scm2js-filename)
(bash echo "(scheme->js ((lambda () " >> $scm2js-filename)
(bash cat $filename >> $scm2js-filename)
(bash echo " #t)))" >> $scm2js-filename)
(bash "scheme" --script $scm2js-filename)
