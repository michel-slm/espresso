(define immediate?
  (lambda (expr)
    (or (number? expr)
        (boolean? expr))))

(define if?
  (lambda (expr)
    (and (list? expr)
         (equal? (car expr) 'if)
         (= (length expr) 4))))
