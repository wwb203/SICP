(define (sqrt-iter guess x)
  (display guess)
  (newline)
  (new-if (good-enough? guess x)
    (+ guess 0.0)
    (sqrt-iter (improve guess x) x)))
(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.01))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt x) (sqrt-iter 1 x))