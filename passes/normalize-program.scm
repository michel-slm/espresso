(define normalize-program
  (lambda (expr)
    (cond
     ((atom? expr)
      `(program ((espresso_main () ,expr))))
     ((equal? (car expr) 'letrec)
      (let ((bindings (cadr expr))
	    (bexprs (cddr expr)))
	`(program ((espresso_main () . ,bexprs)
		   . ,bindings))))
     (else
      (error 'normalize-program
	     "Unknown expression type: ~a" expr)))))
