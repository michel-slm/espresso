(define normalize-predicate
  (lambda (prog)
    (let ((res
           `(program ,(map normalize-predicate:fn (cadr prog)))))
      ;;(printf "normalize-predicate\nbefore: ~s\nafter: ~s\n"
      ;;        prog
      ;;        res)
      res)))

(define normalize-predicate:fn
  (lambda (fn)
    (let ((name (car fn))
          (args (cadr fn))
          (body (caddr fn)))
      `(,name ,args ,((normalize-predicate:expr 'value) body)))))

#|
Test cases:
((normalize-predicate:expr 'test) 42) ==> (not (eq? 42 #f))
((normalize-predicate:expr 'value) '(eq? x y)) ==>
 (if (eq? x y) #t #f)
|#
(define normalize-predicate:expr
  (lambda (context)
    (lambda (expr)
      ((cond
        ((immediate? expr) normalize-predicate:immediate)
        ;; todo
        ((symbol? expr) normalize-predicate:immediate)
        ((if? expr) normalize-predicate:if)
        ((list? expr) normalize-predicate:app)
        (else (error 'normalize-predicate:expr "Unknown expression type: ~a" expr))
        )
       expr context))))

(define normalize-predicate:app
  (lambda (expr context)
    (case context
      ((test)
       (case (car expr)
         ((eq?) `(eq? . ,(map (normalize-predicate:expr 'value) (cdr expr))))
         (else (error 'normalize-predicate:app
                      "Unsupported predicate: ~s" (car expr)))))
      (else
       (case (car expr)
         ((eq?) `(if ,(normalize-predicate:app expr 'test) #t #f))
         (else
          (map (normalize-predicate:expr context) expr)))))))

(define normalize-predicate:if
  (lambda (expr context)
    ;; flip the two branches so we don't need to negate
    (let* ((pre-test (cadr expr))
           (test ((normalize-predicate:expr 'test) pre-test))
           (conseq
            ((normalize-predicate:expr context) (caddr expr)))
           (alt
            ((normalize-predicate:expr context) (cadddr expr))))
      ;;(printf "pre-test: ~s\n" pre-test)
      (cond
       ((and (list? pre-test) (equal? (car pre-test) 'eq?))
        `(if ,test ,conseq ,alt))
       (else `(if ,test ,alt ,conseq))))))


(define normalize-predicate:immediate
  (lambda (expr context)
    (case context
      ((test) `(eq? ,((normalize-predicate:expr 'value) expr) #f))
      (else expr))))






