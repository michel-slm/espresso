(define immediate?
  (lambda (expr)
    (or (number? expr)
        (boolean? expr))))

(define if?
  (lambda (expr)
    (and (list? expr)
         (equal? (car expr) 'if)
         (= (length expr) 4))))

(define prim?
  (lambda (x)
    (case x
      ((fx+ fx-) #t)
      (else #f))))
