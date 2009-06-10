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

(define assemble:app
  (lambda (out)
    (lambda (expr)
      (let* ((arg-regs (map (assemble:arg out) (cdr expr)))
             (res (next-counter "tmp")))
        
        (if (prim? (car expr))
            ((assemble:app:primop out) res (car expr) arg-regs)
            (assemble:app:call res (car expr) arg-regs)))
        )))

(define assemble:app:primop
  (lambda (out)
    (lambda (res op args)
      (if (binary-prim? op)
          (let ((arg2
                 (case op
                   ((fx*) (let ((reg2 (next-counter "shr")))
                            (fprintf out "~a = ashr ~a ~a, 2\n"
                                     reg2 llvm:int (cadr args))
                            reg2))
                   (else (cadr args))))
                (target (case op
                          ((fx/) (next-counter "tmp"))
                          (else res))))
            (fprintf out "~a = " target)
            (fprintf out
                     "~a ~a ~a, ~a\n"
                     (case op
                       ((fx+) 'add)
                       ((fx-) 'sub)
                       ((fx*) 'mul)
                       (else  'sdiv))
                       llvm:int (car args) arg2)
            (case op
              ((fx/) (fprintf out "~a = shl ~a ~a, 2\n"
                              res llvm:int target)))
            res)
          (else
           (error 'assemble:app:primop
                  "Non-binary primops not supported yet: ~a" op))))))

(define assemble:arg
  (lambda (out)
    (lambda (expr)
      (cond
       ((immediate? expr) expr)
       (else ((assemble:expr out) expr))))))

(define assemble:expr
  (lambda (out)
    (lambda (expr)
      (cond
       ((immediate? expr)
        (let* ((t (next-counter "tmp"))
               (res (next-counter "tmp")))
          (fprintf out "~a = alloca ~a\n" t llvm:int)
          (fprintf out "store ~a ~a, ~a* ~a\n" llvm:int expr llvm:int t)
          (fprintf out "~a = load ~a* ~a\n" res llvm:int t)
          res))
       ((list? expr)
        ((assemble:app out) expr))
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
        (reset-counter)
        (fprintf out "define ~a @~a~a {\n"
                 llvm:int name (assemble:function:arglist args))
        ;;(for-each (assemble:expr out) bexprs)
        (fprintf out "entry:\n")
        (let ((res ((assemble:expr out) (car bexprs))))
          (fprintf out "ret ~a ~a\n" llvm:int res))
        (fprintf out "}")))))

(define assemble:function:arglist
  (lambda (args)
    ;; todo
    '()))
