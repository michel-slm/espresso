(define normalize-fixnum
  (lambda (expr)
    (cond
     ((number? expr) (ash expr 2))
     ((boolean? expr) (if expr espresso:true espresso:false))
     ((atom? expr) expr)
     (else
      (map normalize-fixnum expr)))))
