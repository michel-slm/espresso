(define count-fns
  (let ((counter 0))
    (list
     (lambda ()
       (let ((v (format "%~a" counter)))
         (set! counter (add1 counter))
         v))
     (lambda ()
       (set! counter 0)))))

(define make-temp (car count-fns))
(define reset-counter (cadr count-fns))
