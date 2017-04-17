(define (product term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))
(define (inc n)
  (+ n 1))
(define (w-pi n)
  (define a 4)
  (define b (+ 2 (* 2 n)))
  (/ (* 8 (product (lambda (x) (square (/ x (- x 1.0)))) a (lambda (x) (+ x 2)) b)) (+ b 1.0)))
(define (square x) (* x x))
(w-pi 500)