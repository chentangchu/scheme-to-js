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

(define (list-ref-last l)
  (if (null? (cdr l))
      (car l)
      (list-ref-last (cdr l))))

(define (tagged-expr? tag expr)
    (and (pair? expr)
         (eq? (car expr) tag)))

(define (fold l p f)
  (if (null?) p
      (f (car l) (fold (cdr l) p f))))


