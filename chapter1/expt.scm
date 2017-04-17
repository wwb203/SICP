(define (expt x n)
  (cond ((= n 0) 1)
        ((= (mod n 2) 0)
         (square (expt x (/ n 2))))
        (else (* x (expt x (- n 1))))))
(define (square x)
  (* x x))
(define (mod a b)
  (if (< a b) a
    (mod (- a b) b)))
(define (expt-iter b n)
  (expt-iter-sub b n 1))
(define (expt-iter-sub b n a)
  (cond ((= n 0) a)
        ((even? n) (expt-iter-sub (square b) (/ n 2) a))
        (else (expt-iter-sub b (- n 1) (* b a)))
  ))
