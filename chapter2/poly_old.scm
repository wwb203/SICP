(load "coercion.scm")
(load "seq.scm")
(define (install-polynomial-package) 
;;internal procedures
;;representation of poly
(define (make-poly variable term-list)
  (cons variable term-list))
(define (variable p)
  (car p))
(define (term-list p) (cdr p))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) 
       (or (eq? v1 'any) 
	   (eq? v2 'any)
	   (eq? v1 v2))))
(define (non-num-var v1 v2)
  (if (eq? v1 'any)
    v2
    v1))
(define (negative-poly p)
  (make-poly (variable p) 
	     (negative (term-list p))))
(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (non-num-var (variable p1) (variable p2))
	       (add (term-list p1) (term-list p2)))
    (cdr (add (canonical-poly (cons 'polynomial  p1)) 
	 (canonical-poly (cons 'polynomial  p2))))))
(define (equ?-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (equ? (term-list p1) (term-list p2))
    false))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (non-num-var (variable p1) (variable p2))
	       (mul (term-list p1) (term-list p2)))
    (cdr (mul (canonical-poly (cons 'polynomial  p1)) 
	 (canonical-poly (cons 'polynomial  p2))))))
(define (gcd-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (non-num-var (variable p1) (variable p2))
	        (gcd-terms (term-list p1)
					(term-list p2)))))
   
(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (let ((result (div-terms (term-list p1) (term-list p2))))
      (if (null? (cadr result))
	(list (make-poly (non-num-var (variable p1) (variable p2))
		       (cons 'sparse (car result))) 0)
	(list (make-poly (non-num-var (variable p1) (variable p2))
		       (cons 'sparse (car result))) 
	      (make-poly (non-num-var (variable p1) (variable p2))
			 (cadr result)))))
    (error "DIV different variable:" (list p1 p2))))
(define (sub-poly p1 p2)
  (add-poly p1 (negative-poly p2)))

;;interface to rest of the system
(define (tag p) (attach-tag 'polynomial p))
(put 'add '(polynomial polynomial)
     (lambda (p1 p2) (tag (add-poly p1 p2))))
(put 'mul '(polynomial polynomial)
     (lambda (p1 p2) (tag (mul-poly p1 p2))))
(put 'sub '(polynomial polynomial)
     (lambda (p1 p2) (tag (sub-poly p1 p2))))
(put 'div '(polynomial polynomial)
     (lambda (p1 p2) (map tag (div-poly p1 p2))))
(put 'gcd '(polynomial polynomial)
     (lambda (p1 p2) (tag (gcd-poly p1 p2))))
(put 'make 'polynomial
     (lambda (var terms) (tag (make-poly var terms))))
(put 'negative '(polynomial) 
     (lambda (p) (tag (negative-poly p))))
(put '=zero? '(polynomial)
     (lambda (p) (=zero? (term-list p))))
(put 'equ? '(polynomial polynomial)
     (lambda (p1 p2) (equ?-poly p1 p2)))
(put 'raise '(complex)
     (lambda (z) 
       (tag (make-poly 'any 
				 (make-dense-termlist 
				   (list (drop (cons 'complex z))))))))
(put 'raisetag '(complex)
     (lambda (tag) 'polynomial)
     )
'done
)
(install-polynomial-package) 
(define (greatest-common-divisor x y) (apply-generic 'gcd x y))
(define (install-sparse-term-list-package) 

 (define (tag x) (cons 'sparse x))
 (define (the-empty-termlist) '())
 (define (first-term term-list) (car term-list))
 (define (rest-terms term-list) (cdr term-list))
 (define (empty-termlist? term-list) (null? term-list))
 (define (make-term order coeff) (list order coeff))
 (define (order term) (car term))
 (define (coeff term) (cadr term)) 
 (define (=zero-termlist? termlist)
   (or (empty-termlist? termlist)
       (=zero? (coeff (first-term termlist)))))
 (define (adjoin-term term term-list)
  (if (=zero? (coeff term))
    term-list
    (cons term term-list))) 
 (define (equ?-terms L1 L2)
   (cond ((empty-termlist? L1) (empty-termlist? L2))
	 ((empty-termlist? L2) (empty-termlist? L1))
	 (else (let ((t1 (first-term L1))
		     (t2 (first-term L2)))
		 (and (equ? (order t1) (order t2))
		      (equ? (coeff t1) (coeff t2))
		      (equ?-terms (rest-terms L1)
				  (rest-terms L2)))))))
(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
	((empty-termlist? L2) L1)
	(else
	  (let ((t1 (first-term L1))
		(t2 (first-term L2)))
	    (cond ((> (order t1) (order t2))
		   (adjoin-term
		     t1 (add-terms (rest-terms L1) L2)))
		  ((< (order t1) (order t2))
		   (adjoin-term
		     t2 (add-terms L1 (rest-terms L2))))
		  (else
		    (adjoin-term
		      (make-term (order t1)
				 (add (coeff t1) (coeff t2)))
		      (add-terms (rest-terms L1)
				 (rest-terms L2)))))))))
(define (negative-term-list L)
  (if (empty-termlist? L)
    (the-empty-termlist)
    (let ((t (first-term L)))
      (adjoin-term
	(make-term (order t) (negative (coeff t)))
	(negative-term-list (rest-terms L))))))
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
    (the-empty-termlist)
    (add-terms (mul-term-by-all-terms (first-term L1) L2)
	       (mul-terms (rest-terms L1) L2))))
(define (sub-terms L1 L2)
  (add-terms L1 (negative-term-list L2)))
(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
    (the-empty-termlist)
    (let ((t2 (first-term L)))
      (adjoin-term 
      (make-term (+ (order t1) (order t2))
		 (mul (coeff t1) (coeff t2)))
      (mul-term-by-all-terms t1 (rest-terms L))))))
;;interface
(define (dense-to-sparse L)
  (if (empty-termlist? L)
    (the-empty-termlist)
    (if (=zero? (car L))
      (dense-to-sparse (cdr L))
      (adjoin-term
	(make-term (- (length L) 1) (car L))
	(dense-to-sparse (cdr L))))))
(put '=zero? '(sparse) =zero-termlist?)
(put 'add '(sparse sparse) (lambda (L1 L2)
			     (tag  (add-terms L1 L2))))
(put 'sub '(sparse sparse) (lambda (L1 L2)
			     (tag  (sub-terms L1 L2))))
(put 'mul '(sparse sparse) (lambda (L1 L2)
			     (tag  (mul-terms L1 L2))))
(put 'negative '(sparse) (lambda (L)
			     (tag  (negative-term-list L))))
(put 'make-termlist 'sparse (lambda (L) (tag L)))
(put 'raise '(dense) (lambda (L) (tag (dense-to-sparse L))))
(put 'equ? '(sparse sparse) (lambda (L1 L2)
			      (equ?-terms L1 L2)))
(put 'first-term '(sparse) first-term)
(put 'empty-termlist? '(sparse) empty-termlist?)
'done
)
(install-sparse-term-list-package) 
(define (install-dense-term-list-package) 
 (define (tag x) (cons 'dense x))
 (define (the-empty-termlist) '())
 (define (first-coeff term-list) (car term-list))
 (define (rest-coeffs term-list) (cdr term-list))
 (define (empty-termlist? term-list) (null? term-list))
 (define (order termlist) (- (length termlist) 1))
 (define (first-term term-list) (list (order term-list)
				      (first-coeff term-list)))
 (define (adjoin-term term term-list)
    (cons term term-list)) 
 (define (=zero-termlist? termlist)
   (or (empty-termlist? termlist)
       (=zero? (first-coeff termlist))))
 (define (add-terms L1 L2)
   (cond ((empty-termlist? L1) L2)
	 ((empty-termlist? L2) L1)
	 (else
	   (let ((order1 (order L1))
		 (order2 (order L2)))
	     (cond ((> order1 order2)
		    (adjoin-term (first-coeff L1)
				 (add-terms (rest-coeffs L1) L2)))
		   ((< order2 order1)
		    (adjoin-term (first-coeff L2)
				 (add-terms L1 (rest-coeffs L2))))
		   (else
		     (adjoin-term (add (first-coeff L1) (first-coeff L2))
				  (add-terms (rest-coeffs L1)
					     (rest-coeffs L2)))))))))
(define (negative-term-list L)
  (if (empty-termlist? L)
    (the-empty-termlist)
    (adjoin-term (negative (first-coeff L))
	    (negative-term-list (rest-coeffs L)))))
(define (sub-terms L1 L2)
  (add-terms L1 (negative-term-list L2)))
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
    (the-empty-termlist)
    (add-terms (raise-term (order L1) 
			   (mul-term-by-all-terms (first-coeff L1) L2))
	       (mul-terms (rest-coeffs L1) L2))))
 (define (raise-term order termlist)
   (define (list-of-zeros order)
     (if (= order 0)
       '()
       (cons 0 (list-of-zeros (- order 1)))))
   (append  termlist (list-of-zeros order))) 
(define (mul-term-by-all-terms c1 L)
  (if (empty-termlist? L)
    '()
    (adjoin-term (mul c1 (first-coeff L))
	    (mul-term-by-all-terms c1 (rest-coeffs L)))))
;;interface
(put 'add '(dense dense) (lambda (L1 L2)
			     (tag  (add-terms L1 L2))))
(put 'sub '(dense dense) (lambda (L1 L2)
			     (tag  (sub-terms L1 L2))))
(put 'mul '(dense dense) (lambda (L1 L2)
			     (tag  (mul-terms L1 L2))))

(put 'negative '(dense) (lambda (L)
			     (tag  (negative-term-list L))))
(put 'first-term '(dense) first-term)
(put 'make 'dense (lambda (L) (tag L)))
(put '=zero? '(dense) =zero-termlist?)
(put 'empty-termlist? '(dense) empty-termlist?)
'done
)
(install-dense-term-list-package) 
(define (make-dense-termlist x) ((get 'make 'dense) x))
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define d1 (make-polynomial 'x (make-dense-termlist (list 1 2))))
(define d2 (make-polynomial 'x (make-dense-termlist (list 2 1))))
(define s1 (make-polynomial 'x (cons 'sparse (list (list 4 1) (list 3 -1)
						   (list 2 -2) (list 1 2)))))
(define s4 (make-polynomial 'y (cons 'sparse (list (list 1 1) (list 0 1)))))
(define s2 (make-polynomial 'x (cons 'sparse (list (list 3 1) (list 1 -1)))))
(define s3 (make-polynomial 'y (cons 'sparse (list (list 1 s2) (list 0 2)))))


(define z1 (make-complex-from-real-imag 1 2))
(define (expand-poly p)
  (define (termlist L)
    (if (eq? (car L) 'sparse) (cdr L) 
      (cdr (raise L))))
  (cond ((null? p) '())
	((num? p) (list (list 'any p)))
	((eq? (type-tag p) 'polynomial)
	 (let ((var (cadr p))
	       (L (termlist (cddr p))))
	  (flatmap (lambda (term)
		(map (lambda (Le) (append (list var (car term)) Le)) 
			(expand-poly (cadr term)))) L))) 
	(else "unkown type EXPAND-POLY" p))) 
(define var-order (list 'x 'y))
(define (fold-term L)
  (define (collect-order var L)
    (if (null? L) 0
      (let ((Lvar (car L))
	    (Lorder (cadr L)))
	(if (eq? Lvar var)
	  (+ Lorder (collect-order var (cddr L)))
	  (collect-order var (cddr L))))))
  (define (collect-num L)
    (if (null? L) 1
      (let ((Lvar (car L))
	    (Lcoeff (cadr L)))
	(if (eq? Lvar 'any)
	  (mul Lcoeff (collect-num (cddr L)))
	  (collect-num (cddr L))))))
  (append  
  (map (lambda (var)  (collect-order var L)) var-order)
  (list (collect-num L)))) 
(define (fold-termlist L)
  (map fold-term L))
(define (adjoin-by-first-order item item-seq)
  (if (null? item-seq)
    (list item)
    (if (> (car item) (car (car item-seq)))
      (cons item item-seq)
      (cons (car item-seq) 
	    (adjoin-by-first-order item (cdr item-seq))))))
(define (sort-termlist termlist)
  (if (null? termlist)
    '()
    (adjoin-by-first-order (car termlist)
			   (sort-termlist (cdr termlist))
			   )))
(define (make-polynomial-from-termlist termlist var-list)
  (if (null? var-list)
   (car termlist) 
	(make-polynomial (car var-list)
			 (cons 'sparse
			      (list  (list (car termlist)
				     (make-polynomial-from-termlist
				       (cdr termlist)
				       (cdr var-list))))))))
(define (poly-sum-termlist termlist)
  (cond ((null? termlist) '())
	((= (length termlist) 1) 
	 (make-polynomial-from-termlist (car termlist) var-order))
	(else
	  (add  
	   (make-polynomial-from-termlist (car termlist) var-order) 
	   (poly-sum-termlist (cdr termlist))))))
(define (canonical-poly poly)
  (poly-sum-termlist (fold-termlist (expand-poly poly))))
(define (first-term L) (apply-generic 'first-term L))
(define (order term) (car term))
(define (coeff term) (cadr term))
(define (the-empty-termlist) (cons 'sparse '()))
(define (empty-termlist? L) 
  (apply-generic 'empty-termlist? L))
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
    term-list
    (cons term term-list))) 
(define (div-terms L1 L2)
  (define (adjoin item seq)
    (if (null? seq)
      item
      (cons item seq)))
  (if (empty-termlist? L1)
    (list (the-empty-termlist) (the-empty-termlist))
    (let ((t1 (first-term L1))
	  (t2 (first-term L2)))
      (if (> (order t2) (order t1))
	     (list (the-empty-termlist) L1)
	     (let ((new-c (div (coeff t1) (coeff t2)))
		   (new-o (- (order t1) (order t2))))
	       (let ((rest-of-result
		       (div-terms (sub L1 (mul (cons 'sparse 
					  (list (list new-o new-c)))
				    L2)) L2)))
		 (newline)
		 (display "div-terms")
		 (display rest-of-result)
		(list (cons (list new-o new-c) (car rest-of-result))
		      (cadr rest-of-result)))))))) 
(define (gcd-terms a b)
 (if (empty-termlist? b)
   a
   (gcd-terms b (remainder-terms a b))
   ))
(define (remainder-terms a b)
  (let ((t1 (first-term a))
	(t2 (first-term b)))
    (cadr (div-terms (cddr (mul (make-polynomial 'x a) 
			  (expt (coeff t2) 
				(+ 1 (- (order t1) (order t2)))))) b))))
(define rf (make-rational s2 s1))
(define p1 (make-polynomial 'x (cons 'sparse (list (list 2 1) (list 1 -2)
						   (list 0 1)))))
(define p2 (make-polynomial 'x (cons 'sparse (list (list 2 11) 
						   (list 0 7)))))
(define p3 (make-polynomial 'x (cons 'sparse (list (list 1 13) (list 0 5)))))
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

