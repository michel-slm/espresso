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

