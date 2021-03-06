(load "expmod.lisp")
(define (miller-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1)
  (try-it (+ 1 (random (- n 2)))))
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((miller-test n) (fast-prime? n (- times 1)))
        (else #f)))
