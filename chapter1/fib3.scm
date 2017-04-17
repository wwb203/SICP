(define (fib3-recur n)
  (if (< n 3) n
    (+ (fib3-recur (- n 1))
       (* 2 (fib3-recur (- n 2)))
       (* 3 (fib3-recur (- n 3))))))
(define (fib3-iter n)
  (define (fib3-iter-sub x1 x2 x3 n)
    (if (= n 0) x1 
      (fib3-iter-sub x2 x3 (+ (* 3 x1) (* 2 x2) x3) (- n 1))))
  (fib3-iter-sub 0 1 2 n))
