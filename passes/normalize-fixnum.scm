(define normalize-fixnum
  (lambda (expr)
    (cond
     ((number? expr) (ash expr 2))
     ((atom? expr) expr)
     (else
      (map normalize-fixnum expr)))))
