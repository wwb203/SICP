(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))
(define (head item x)
  (define (iter item x answer)
    (cond ((null? x) #f)
          ((eq? item (car x)) answer)
          (else (iter item (cdr x) (append (list (car x)) answer)))))
  (iter item x '()))
(define (quote-equal? list1 list2)
  (cond ((and (null? list1)
              (null? list2)) #t)
        ((and (pair? list1)
              (pair? list2))
         (and (quote-equal? (car list1) (car list2))
              (quote-equal? (cdr list1) (cdr list2))))
        ((and (not (pair? list1))
              (not (pair? list2))
              (eq? list1 list2)))
        (else #f)))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknown expression type: DERIV" exp))))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) 
        (cond ((=number?  a2 0) a1)
              ((=number?  a1 0) a2)
              ((and (number? a1) (number? a2)) (+ a1 a2))
              (else (list a1 '+ a2))))
(define (=number? exp num)
  (if (number? exp)
    (= exp num)
    #f))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(define (sum? x) (memq '+ x))
(define (addend s) 
  (let ((end (head '+ s)))
        (if (null? (cdr end)) (car end) end)))
(define (augend s)
  (let ((aug (cdr (memq '+ s))))
    (if (null? (cdr aug)) (car aug) aug)))
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplicand s) 
  (if (null? (cdddr s))
    (caddr s)
    (cddr s))) 
(define (multiplier p) (car p))

