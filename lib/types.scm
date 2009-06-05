(define immediate?
  (lambda (expr)
    (or (number? expr)
        (boolean? expr))))
