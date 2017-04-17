(define (make-accumulator value)
  (define (update amount)
    (set! value (+ value amount))
    value)
  update
  )
