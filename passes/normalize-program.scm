(define normalize-program
  (lambda (expr)
    (cond
     ((and (list? expr)
           (equal? (car expr) 'letrec))
      (let ((bindings (cadr expr))
	    (bexprs (cddr expr)))
	`(program ((espresso_main () . ,bexprs)
		   . ,(map normalize-program:fn bindings)))))
     (else
      `(program ((espresso_main () ,expr))))
     )))

(define normalize-program:fn
  (lambda (fnbinding)
    (let* ((fn-name (car fnbinding))
           (fn-expr (cadr fnbinding))
           (fn-args (map (lambda (arg)
                           (list arg
                                 (string->symbol
                                  (format "%~a" arg))))
                         (cadr fn-expr)))
           (fn-body (caddr fn-expr)))
      ;; hack: ignore closure
      (list fn-name (map cadr fn-args)
            (normalize-program:rename-args fn-body fn-args)))))

(define normalize-program:rename-args
  (lambda (expr fn-args)
    (cond
     ((symbol? expr)
      (let ((res (assoc expr fn-args)))
        (if res
            (cadr res)
            expr)))
     ((immediate? expr) expr)
     (else ;; assuming a list
      (map (lambda (e)
             (normalize-program:rename-args e fn-args))
           expr)))))

                   
            

                   