
(define count-fns
  (let ((counters '()))
    (letrec ((incr-counter
              (lambda (name)
                (let loop ((counters counters))
                  (cond
                   ((null? counters)
                    (list (list name 1)))
                   ((equal? (caar counters) name)
                    (cons (list name (add1 (cadar counters)))
                          (cdr counters)))
                   (else
                    (cons (car counters) (loop (cdr counters))))))))
             (next-counter
              (lambda (name)
                (let ((res (assoc name counters)))
                  (set! counters (incr-counter name))
                  (format "%~a~a" name (if res (cadr res) ""))))))
    (list next-counter 
     (lambda ()
       (set! counters '()))))))

(define next-counter (car count-fns))
(define reset-counter (cadr count-fns))
