(define (sum-max2 x y z)
  (cond ((AND (< x y) (< x z)) (+ y z))
        ((AND (< y x) (< y z)) (+ x z))
        ((AND (< z x) (< z y)) (+ x y))))
