(define nil '())
(define square (lambda (x) (* x x)))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
  (if (null? sequence) initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))
(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))
(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(define (sum-odd-squares tree)
  (accumulate
    + 0 (map square (filter odd? (enumerate-tree tree)))))
(sum-odd-squares (list 1 (list 2 (list 3 4)) 5))
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(map (lambda (x) (* x x)) (list 1 2 3))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(length (list 1 2 3 4 5))
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))
(define (count-leaves t)
  (accumulate + 0 (map (lambda (tree)
                         (if (not (pair? tree))
                                  1
                                  (count-leaves tree))) t)))
(count-leaves (list 1 (list 2 (list 3 4))))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))
(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(define v (list 1 2 3 4))
(define (dot-product v w)
  (let ((seqs (list v w)))
    (accumulate + 0 (accumulate-n * 1 seqs))))
(define (matrix-*-vector m v)
  (accumulate (lambda (w m-rest) (cons (dot-product w v) m-rest)) nil m))
(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define (transpose m)
  (accumulate-n cons nil m))
(transpose m)
(define (matrix-*-matrix m1 m2)
  (accumulate (lambda (w rest) (cons 
                                 (accumulate (lambda (v rest)
                                               (cons (dot-product v w) rest)) nil (transpose m2)) rest)) nil m1))
(define m1 (list (list 1 2) (list 3 4)))
(define m2 (list (list 1 1) (list 0 1)))
(matrix-*-matrix m1 m2)
(define (fold-left op initial  sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))
(define fold-right accumulate)
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(reverse (list 1 2 3))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
(reverse (list 1 2 3))
(define (unique-pair n)
(accumulate
  append nil (map (lambda (i)
                    (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(load "division.scm")
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                 (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))))
(define (permutations s)
  (if (null? s)
    (list nil)
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))))
             s)))
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
(permutations (list 1 2 3))
(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))
(unique-pairs 3)
(define (unique-triplet n)
  (flatmap
    (lambda (i)
      (map (lambda (pair) (append (list i) pair)) (unique-pairs (- i 1))))
      (enumerate-interval 1 n)))
(define (sum-eq-triplet n s)
  (filter (lambda (triplet) (= (list+ triplet) s))
          (unique-triplet n)))
(define (list+ sequence)
  (accumulate + 0 sequence))
(sum-eq-triplet 8 8)
(define (queen-puzzle n)
  (solution n n))
(define (solution i n)
  (if (= i 1)
    (map (lambda (i) (list i)) (enumerate-interval 1 n)) 
    (accumulate append nil (map (lambda (sol) (add-queen sol n)) (solution (- i 1) n)))))
(define (add-queen solution n)
  (map (lambda (j) (cons j solution)) (filter (lambda (j) (safe? j solution)) (enumerate-interval 1 n))))
(define (add-row solution)
(accumulate-n cons nil (list (reverse (enumerate-interval 1 (length solution))) solution)))
(define (safe? j solution)
  (define (safe-sub pos1 pos2)
    (let ((r1 (car pos1))
          (c1 (cadr pos1))
          (r2 (car pos2))
          (c2 (cadr pos2)))
          (and (not (= c1 c2))
               (not (= (- r1 r2) (abs (- c1 c2)))))))
  (let ((pos1 (list (+ 1 (length solution)) j)))
  (accumulate (lambda (tf rest)
                (if tf
                  rest
                  #f)) #t (map (lambda (pos2) (safe-sub pos1 pos2)) (add-row solution)))))
;(define test-solution (list 1 5))
;(length (queen-puzzle 8))
