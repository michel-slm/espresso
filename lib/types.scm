(define immediate?
  (lambda (expr)
    (or (number? expr)
        (boolean? expr)
        (null? expr))))

(define if?
  (lambda (expr)
    (and (list? expr)
         (equal? (car expr) 'if)
         (= (length expr) 4))))

(define pred?
  (lambda (x)
    (or (binary-pred? x))))

(define binary-pred?
  (lambda (x)
    (case x
      ((eq? < <= = >= >) #t)
      (else #f))))

(define prim?
  (lambda (x)
    (or (binary-prim? x))))

(define binary-prim?
  (lambda (x)
    (case x
      ((eq? fx+ fx- fx* fx/ remainder) #t)
      (else #f))))
