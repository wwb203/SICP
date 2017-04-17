(define (map proc items)
 (if (null? items)
   '()
   (cons (proc (car items))
         (map proc (cdr items)))))
(map abs (list -10 2.5 -11.6 17))
(define square (lambda (x) (* x x)))
(define (square-list items)
  (if (null? items)
   '()
   (cons (square (car items))
          (square-list (cdr items)))))
(define (square-list items)
  (map square items))
(square-list (list 1 2 3))
(define (for-each proc items)
  (cond ((null? items) #t)
    (else  (proc (car items))
    (for-each proc (cdr items)))))
(for-each (lambda (x)
            (newline)
            (display x)) (list 57 321 88))
(define (reverse items)
  (cond ((null? items) '())
        ((not (pair? items)) items)
        (else 
          (list (reverse (cadr items)) (reverse (car items))))))

(define x (list (list 1 2) (list 3 4)))
(reverse x)
(define (fringe items)
  (cond ((null? items) '())
        ((not (pair? items)) (list items))
        (else (append (fringe (car items))
                (fringe (cdr items))))))
(fringe x)
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else
          (cons (square-tree (car tree))
                (square-tree (cdr tree))))))
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree sub-tree)
           (* sub-tree sub-tree))) tree))
(square-tree (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair?  sub-tree)
           (tree-map proc sub-tree)
           (proc tree))) tree))
(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (items) (cons (car s) items)) rest)))))
(subsets (list 1 2 3))
