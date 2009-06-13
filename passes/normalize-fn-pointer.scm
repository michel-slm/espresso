(define normalize-fn-pointer
  (lambda (prog)
    `(program ,(map normalize-fn-pointer:fn (cadr prog)))))

(define normalize-fn-pointer:fn
  (lambda (fn)
    (let ((name (car fn))
          (args (cadr fn))
          (body (caddr fn)))
      `(,name ,args ,(normalize-fn-pointer:expr args body)))))

(define normalize-fn-pointer:expr
  (lambda (fn-args expr)
    (cond
     ((immediate? expr) expr)
     ((symbol? expr) expr)
     ((if? expr) `(if . ,(map (lambda (expr)
                                (normalize-fn-pointer:expr fn-args expr))
                              (cdr expr))))
     ((list? expr)
      (let ((op (car expr))
            (rands (map (lambda (expr)
                          (normalize-fn-pointer:expr fn-args expr))
                        (cdr expr))))
        (if (member op fn-args)
            `((inttoptr ,op ,(length rands)) . ,rands)
            `(,op . ,rands))))
     (else (error 'normalize-fn-pointer:expr
                  "Unknown expression type: ~a" expr)))))
