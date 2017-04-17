(define (sine x)
  (if (< (abs x) 0.1) x
    (p (sine (/ x 3.0)))))
(define (cubic x)
  (* x x x ))
(define (p x)
  (- (* 3 x) (* 4 (cubic x))))

