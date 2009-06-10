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
