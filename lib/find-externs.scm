(define find-externs
  (lambda (prog)
    (let* ((fns (cadr prog))
           (fn-names (map car fns))
           (finder (find-externs:expr fn-names)))
      (fold-right (lambda (fn externs)
                    (let ((body (caddr fn)))
                      (finder body externs)))
                  '()
                  fns))))

(define find-externs:expr
  (lambda (fn-names)
    (lambda (expr externs)
      (cond
       ((immediate? expr) externs)
       ((symbol? expr) externs)
       ((if? expr)
        ;; by this time, the test would be eq?
        ;; just check conseq and alt
        (let ((conseq (caddr expr))
              (alt (cadddr expr)))
          ((find-externs:expr fn-names)
           conseq
           ((find-externs:expr fn-names) alt externs))))
       ((list? expr)
        (let ((op (car expr))
              (args-res
               (fold-right (find-externs:expr fn-names)
                           externs
                           (cdr expr))))
          (if (or (prim? op)
                  (inttoptr? op)
                  (member op fn-names))
              args-res
              ;; an extern we've seen before
              (if (member op args-res)
                  args-res
                  (cons op args-res)))))
       (else
        (error 'find-externs:expr
               "Unknown expression type: ~s" expr))))))


                         