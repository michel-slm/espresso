(define normalize-predicate
  (lambda (prog)
    `(program ,(map (normalize-predicate:fn 'value) (cadr prog)))))

(define normalize-predicate:fn
  (lambda (context)
    (lambda (fn)
      (let ((name (car fn))
            (args (cadr fn))
            (bexprs (cddr fn)))
        `(,name ,args . ,(map (normalize-predicate:expr 'value) bexprs))))))
        
(define normalize-predicate:expr
  (lambda (context)
    (lambda (expr)
      ((cond
        ((immediate? expr) normalize-predicate:immediate)
        ((if? expr) normalize-predicate:if)
        ((list? expr) normalize-predicate:app)
        (else (error 'normalize-predicate "Unknown expression type: ~a" expr))
        )
       expr context))))

(define normalize-predicate:app
  (lambda (expr context)
    (map (lambda (x) (normalize-predicate x context)) expr)))

(define normalize-predicate:if
  (lambda (expr context)
    (let ((test (normalize-predicate (cadr expr) 'boolean)))
      `(if ,test
	   ,(normalize-predicate (caddr expr) context)
	   ,(normalize-predicate (cadddr expr) context)))))

(define normalize-predicate:immediate
  (lambda (expr context)
    (case context
      ((boolean) `(not (equal? ,((normalize-predicate:expr 'value) expr)) espresso:false))
      (else
       (cond
        ((boolean? expr) (if expr espresso:true espresso:false))
        (else expr))))))







