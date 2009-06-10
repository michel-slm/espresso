(define normalize-immediate
  (lambda (prog)
    `(program ,(map normalize-immediate:fn (cadr prog)))))

(define normalize-immediate:fn
  (lambda (fn)
    (let ((name (car fn))
          (args (cadr fn))
          (body (caddr fn)))
      `(,name ,args ,(normalize-immediate:expr body)))))

(define normalize-immediate:expr
  (lambda (expr)
    (cond
     ((number? expr) (ash expr 2))
     ((boolean? expr) (if expr espresso:true espresso:false))
     ((null? expr) espresso:nil)
     ((atom? expr) expr)
     (else
      (map normalize-immediate:expr expr)))))
