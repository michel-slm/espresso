(define next-collatz
  (lambda (n)
    (compile-program
     `(if (eq? (remainder ,n 2) 0)
          (fx/ ,n 2)
          (fx+ (fx* 3 ,n) 1)))))

;; using parent Scheme's recursion
(define collatz-sequence1
  (lambda (n)
    (if (= n 1)
        (list n)
        (cons n (collatz-sequence1 (next-collatz n))))))

(define mksrc-collatz-sequence
  (lambda (n)
    `(letrec ((collatz-sequence
               (lambda (n)
                 (if (eq? n 1)
                     (cons 1 ())
                     (cons n
                           (collatz-sequence (next-collatz n))))))
              (next-collatz
               (lambda (n)
                 (if (eq? (remainder n 2) 0)
                     (fx/ n 2)
                     (fx+ (fx* 3 n) 1)))))
       (collatz-sequence ,n))))
    
(define collatz-sequence2
  (lambda (n)
    (compile-program
     (mksrc-collatz-sequence n))))

(define evenp
  (lambda (n)
    (compile-program
     `(letrec ((evenp (lambda (n)
                        (if (eq? n 0)
                            #t
                            (oddp (fx- n 1)))))
               (oddp (lambda (n)
                       (if (eq? n 0)
                           #f
                           (evenp (fx- n 1))))))
        (evenp ,n)))))

(define fac
  (lambda (n)
    (compile-program
     `(letrec ((fac (lambda (n)
                      (if (eq? n 0)
                          1
                          (fx* n (fac (fx- n 1)))))))
        (fac ,n)))))

(define fib
  (lambda (n)
    (compile-program
     `(letrec ((fib (lambda (n)
                      (if (< n 2)
                          n
                          (fx+ (fib (fx- n 1))
                               (fib (fx- n 2)))))))
        (fib ,n)))))

(define fib-iter
  (lambda (n)
    (compile-program
     `(letrec ((fib (lambda (a b n)
                      (if (eq? n 0)
                          a
                          (fib b (fx+ a b) (fx- n 1))))))
        (fib 0 1 ,n)))))

(define emap
  (lambda (p-expr ls-expr)
    (compile-program
     `(letrec ((p ,p-expr))
        (map p ,ls-expr)))))

(define efilter
  (lambda (p-expr ls-expr)
    (compile-program
     `(letrec ((p ,p-expr))
        (filter p ,ls-expr)))))
