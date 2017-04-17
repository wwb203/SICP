(define (fib n)
  (define (fib-iter x1 x2 n)
    (if (= n 0)
      x1
      (fib-iter x2 (+ x1 x2) (- n 1))))
  (fib-iter 0 1 n))
