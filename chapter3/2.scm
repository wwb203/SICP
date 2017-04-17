(define (make-monitored procedure)
  (define mf 
    (let ((counter 0))
      (lambda (m)
        (cond ((eq? m 'how-many-calls?) counter)
              ((eq? m 'reset-count) (set! counter 0))
              (else (begin (set! counter (+ 1 counter))
                           (procedure m)))))))
  mf)
