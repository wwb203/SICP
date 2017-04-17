(define (sum-1-n n)
  (if (< n 1)
    0
    (+ n (sum-1-n (- n 1)))))
(sum-1-n 4)
