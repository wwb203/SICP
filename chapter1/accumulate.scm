(load "division.lisp")
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))
(define (filtered-accumulate combiner filter null-value term a next b)
  (define (apply-filter a)
    (if (filter a) (term a)
      null-value))
  (if (> a b)
    null-value
    (combiner (apply-filter a) (filtered-accumulate
                                 combiner
                                 filter
                                 null-value
                                 term
                                 (next a)
                                 next
                                 b))))
(define (sumprime a b) (filtered-accumulate + prime? 0 (lambda (x) x) a (lambda (x) (+ x 1)) b))
(sumprime 2 5)
