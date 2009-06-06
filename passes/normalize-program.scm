(define normalize-program
  (lambda (expr)
    (cond
     ((and (list? expr)
           (equal? (car expr) 'letrec))
      (let ((bindings (cadr expr))
	    (bexprs (cddr expr)))
	`(program ((espresso_main () . ,bexprs)
		   . ,bindings))))
     (else
      `(program ((espresso_main () ,expr))))
     )))
