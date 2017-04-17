(define (make-table)
  (list '*table*))
(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records))
	 (car records))
	(else (assoc key (cdr records)))))
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      false)))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
		(cons (cons key value)
		      (cdr table)))))
  'ok)
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record
	      (assoc key-2 (cdr subtable))))
	(if record (cdr record) false))
      false)))
(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record
	      (assoc key-2 (cdr subtable))))
	(if record
	  (set-cdr! record value)
	  (set-cdr!
	    subtable
	    (cons (cons key-2 value)
		  (cdr subtable)))))
      (set-cdr!
	table
	(cons (list key-1 (cons key-2 value))
	      (cdr table)))))
  'ok)
(define (make-table)
  (let ((local-table (list '*table*)))
	(define (assoc key records)
	  (cond ((null? records) false)
		((same-key? key (caar records))
		 (car records))
	  (else (assoc key (cdr records)))))
    (define (same-key? key record)
      (cond ((and (number? key) (number? record))
	     (< (abs (- key record)) 0.1))
	    (else
	      (equal? key record))))
    (define (lookup key-1 key-2)
      (let ((subtable
	      (assoc key-1 (cdr local-table))))
	(if subtable
	  (let ((record
		  (assoc key-2 cdr subtable)))
	    (if record (cdr record) false))
	  false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
	      (assoc key-1 (cdr local-table))))
	(if subtable
	  (let ((record
		  (assoc key-2 (cdr subtable))))
	    (if record
	      (set-cdr! record value)
	      (set-cdr!
		subtable
		(cons (cons key-2 value)
		      (cdr subtable)))))
	  (set-cdr!
	    local-table
	    (cons (list key-1
			(cons key-2 value))
		  (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else
	      (error "Unknown operation:
		     TABLE" m))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define (make-nd-table)
  (list '*table*))
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record
	      (assoc key-2 (cdr subtable))))
	(if record (cdr record) false))
      false)))
(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record
	      (assoc key-2 (cdr subtable))))
	(if record
	  (set-cdr! record value)
	  (set-cdr!
	    subtable
	    (cons (cons key-2 value)
		  (cdr subtable)))))
      (set-cdr!
	table
	(cons (list key-1 (cons key-2 value))
	      (cdr table)))))
  'ok)

(define (lookup-nd key-list table)
  (if (null? key-list) (cdr table)
    (let ((subtable (assoc (car key-list) (cdr table))))
      (if subtable
	(lookup-nd (cdr key-list) subtable)
	false))))
(define (insert-nd! key-list value table)
  (if (null? key-list)
    (set-cdr! table value)
    (let ((subtable (assoc (car key-list) (cdr table))))
      (if subtable
	(insert-nd! (cdr key-list) value subtable)
	(let ((new-subtable (list (car key-list))))
	  (let ((new-branch (cons new-subtable (cdr table))))
	    (set-cdr! table new-branch)
	    (insert-nd! (cdr key-list) value new-subtable))))))
  'ok)
(define (make-tree key value left-branch right-branch)
  (cons (cons key value) (cons left-branch right-branch)))
(define (key-tree tree) (caar tree))
(define (value-tree tree) (cdar tree))
(define (left-branch tree) 
  (if (null? tree) '() 
  (cadr tree)))
(define (right-branch tree) 
  (if (null? tree) '() 
  (cddr tree)))
(define (set-left-branch! tree new-tree)
  (set-car! (cdr tree) new-tree))
(define (set-right-branch! tree new-tree)
  (set-cdr! (cdr tree) new-tree))
(define (set-value-tree! tree value)
  (set-cdr! (car tree) value))
(define (insert-tree! key value tree)
  (let ((new-tree (make-tree key value '() '())))
   (cond ((eq? key (key-tree tree)) (set-value-tree! tree value))
	 ((< key (key-tree tree))
	  (if (null? (left-branch tree))
		     (set-left-branch! tree new-tree)
		     (insert-tree! key value (left-branch tree))))
	((> key (key-tree tree))
	  (if (null? (right-branch tree))
		     (set-right-branch! tree new-tree)
		     (insert-tree! key value (right-branch tree)))))))
(define (lookup-tree key tree)
  (if (null? tree) false
    (let ((this-entry (key-tree tree)))
      (cond ((eq? key this-entry) (value-tree tree))
	    ((< key this-entry) (lookup-tree key (left-branch tree)))
	    (else (lookup-tree key (right-branch tree)))))))
