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
             (res (make-temp)))
        
        (fprintf out "~a = " res)
        (fprintf out "~a\n"
                 (if (prim? (car expr))
                     (assemble:app:primop (car expr) arg-regs)
                     (assemble:app:call (car expr) arg-regs)))
        res
        ))))

(define assemble:app:primop
  (lambda (op args)
    (case op
      ((fx+) (format "add ~a ~a, ~a\n" llvm:int (car args) (cadr args)))
      (else (error 'assemble:app:primop
                   "Unknown primop: ~a" op)))))

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
        (let* ((t (make-temp))
               (res (make-temp)))
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
