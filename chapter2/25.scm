(load "expt.lisp")
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))
(define (car pair)
  (define (iter pair a)
    (if (> (remainder pair 2) 0)
      a
      (iter (/ pair 2) (+ a 1))))
  (iter pair 0))
(define (cdr pair)
  (define (iter pair a)
    (if (> (remainder pair 3) 0)
      a
      (iter (/ pair 3) (+ a 1))))
  (iter pair 0))


