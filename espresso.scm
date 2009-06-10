(load "lib/constants.scm")
(load "lib/types.scm")
(load "lib/assemble.scm")
(load "lib/find-externs.scm")

(load "lib/utils.scm")

(load "tests.scm")

(define passes)

(define-syntax enable-passes
  (syntax-rules ()
    ((_ pass* ...)
     (begin
       (for-each (lambda (pass)
                   (load (format "passes/~a.scm" pass)))
                 '(pass* ...))
       (set! passes (list 'pass* ...))))))

(enable-passes normalize-program
               normalize-predicate
               normalize-immediate
               )

(define apply-passes
  (lambda (expr)
    (let loop ((passes passes)
               (expr expr))
      (if (null? passes)
          expr
          (loop (cdr passes) ((eval (car passes)) expr))))))

(define compile-program
  (lambda (expr . opts)
    (let ((name (if (null? opts) 'tmp (car opts))))
      (assemble name (apply-passes expr))
      (system (format "llvm-as -f -o=~a.o.bc ~a.ll" name name))
      (system (format "llvm-link -f -o=~a.bc ~a.o.bc lib/espresso.o.bc" name name))
      (run-program name))))

(define compile-c-program
  (lambda (name)
    (system (format "clang -emit-llvm -c -o ~a.o.bc ~a.c" name name))
    ;; also generate IR representation for debugging
    (system (format "clang -emit-llvm -S -o ~a.ll ~a.c" name name))
    (system (format "llvm-link -f -o=~a.bc ~a.o.bc lib/espresso.o.bc" name name))
    (run-program name)))

(define run-program
  (lambda (name)
    (let ((proc (process (format "lli ~a.bc" name))))
      (read (car proc)))))

