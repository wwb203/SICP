(load "putget.scm")
(load "complex-package.scm")
(load "arithmetic.scm")
(define (apply-generic op . args)
  (newline)
  (display "apply-generic")
  (display op)
  (display "->")
  (display args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	(if (or (eq? op 'sub) (eq? op 'div) (eq? op 'add) (eq? op 'mul) 
		(eq? op 'make-rational) 
	(eq? op 'make-complex-from-real-imag)
		(eq? op 'make-complex-from-mag-ang))
	(drop (apply proc  (map contents args))) 
	 (apply proc (map contents args)))
	(if (= (length args) 2)
	    (let ((CL (compare-level type-tags)))
	      (cond ((eq? CL 'SAME)
		     (error "No method for types" (list op type-tags)))
		    ((eq? CL 'RAISE1) 
		      (apply-generic op (raise (car args)) (cadr args)))
		    ((eq? CL 'RAISE2) 
		     (apply-generic op (car args) (raise (cadr args))))
		    )) 
	    (error "No method for these types" (list op type-tags)))))))  
(define (install-raise-package)
  (put 'raise '(integer) 
		(lambda (n)  ((get 'make 'rational) n 1)))
  (put 'raisetag 'integer
       (lambda ()  'rational))
  (put 'raise '(rational) 
		(lambda (r)
		  (make-real (/ (+ 0.0 ((get 'numer '(rational)) r))
			       (+ 0.0 ((get 'denom '(rational)) r))))))
  (put 'raisetag 'rational
       (lambda () 'real))

  (put 'raise '(real) 
		(lambda (x)  (make-complex-from-real-imag x 0)))
  (put 'raisetag 'real
       (lambda () 'complex))
  'done)
(define (raise x) 
  (apply-generic 'raise x))
(define (raisetag x)
  (apply-generic 'raisetag x))
(install-raise-package)
(define (install-projection)
  (define (project-complex z)
 	(make-real (real-part z)))
  (define (project-real x)
        (make-integer (inexact->exact (round x))))
  (define (project-rational r)
          (car r))  
  (put 'project '(complex) project-complex)
  (put 'project '(real) project-real)
  (put 'project '(rational) project-rational)
  'done
 )
(install-projection)
(define (project x) 
  (apply-generic 'project x))
(define (drop x)
  (if (get 'project (list (type-tag x)))
    (if (equ? x (project x))
      (drop (project x))
      x)
    x))
(define (compare-level tags)
  (newline)
  (display "compare-level")
  (display tags)
  (define (level tag)
    (if (get 'raisetag tag)
      (+ 1 (level ((get 'raisetag tag))))
      0
      )
    )
  (let ((level1 (level (car tags)))
	(level2 (level (cadr tags))))
    (cond ((> level1 level2) 'RAISE1)
	  ((< level1 level2) 'RAISE2)
	  ((= level1 level2) 'SAME))))
