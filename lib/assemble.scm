(define assemble
  (lambda (name expr)
    (let ((out (open-output-file (format "~a.ll" name) 'replace)))
      (assemble:program out expr)
      (close-output-port out))))

(define assemble:program
  (lambda (out expr)
    (cond
     ((and (list? expr) (equal? (car expr) 'program))
      (for-each (assemble:function out) (cadr expr)))
     (else
      (error 'assemble:program
             "Invalid program: ~a" expr)))))

(define assemble:expr
  (lambda (out)
    (lambda (expr)
      (cond
       ((immediate? expr)
        (fprintf out "ret ~a ~a\n" llvm:int expr))
       (else
        (error 'assemble:expr
               "Unknown expression: ~a" expr))))))

(define assemble:function
  (lambda (out)
    (lambda (fn)
      (let ((name (car fn))
            (args (cadr fn))
            (bexprs (cddr fn))
            )
        (fprintf out "define ~a @~a~a {\n" llvm:int name (assemble:function:arglist args))
        (for-each (assemble:expr out) bexprs)
        (fprintf out "}")))))

(define assemble:function:arglist
  (lambda (args)
    ;; todo
    '()))
