(define put 2d-put!)
; 2d-get doesn't work because it uses eq?
(define (get x-key y-key) 
    (let ((1d-table (2d-get-alist-x x-key)))
          (let ((type-f (assoc y-key 1d-table)))
	          (if type-f (cdr type-f) false))))
(define put-coercion put)
(define get-coercion get)
