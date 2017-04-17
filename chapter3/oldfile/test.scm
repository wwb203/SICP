(define memoize
  (lambda (f)
   ((lambda (table)
      (lambda (x)
	((lambda (previous-result)
	   (or previous-result
	       ((lambda (result)
		  (insert! x result table)
		  result) (f x)))) 
	 (lookup x table)))) 
    (make-table))))
(define memo-fib
  (lambda (f)
   ((lambda (table)
      (lambda (x)
	((lambda (previous-result)
	   (or previous-result
	       ((lambda (result)
		  (insert! x result table)
		  result) (f x)))) 
	 (lookup x table)))) 
    (make-table)))
  (lambda (n)
    (cond ((= n 0) 0)
	  ((= n 1) 1)
	  (else
	    (+ (+ (memo-fib (- n 1))
		  (memo-fib (- n 2))))))))
(define memo-fib
 (lambda (x)
	((lambda (previous-result)
	   (or previous-result
	       ((lambda (result)
		  (insert! x result table)
		  result) ((lambda (n)
    (cond ((= n 0) 0)
	  ((= n 1) 1)
	  (else
	    (+ (+ (memo-fib (- n 1))
		  (memo-fib (- n 2))))))) x)))) 
	 (lookup x table))))  
