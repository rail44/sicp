(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (my-length items)
  (if (null? items)
    0
    (+ 1 (my-length (cdr items)))))

(define (my-length2 items)
  (define (length-iter a count)
    (if (null? a)
      count
      (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (test-my-length)
  (define odds (list 1 3 5 7))
  (assert
    (=
      (length odds)
      (my-length odds)
      (my-length2 odds))))

(define (my-append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (test-my-append)
  (define odds (list 1 3 5 7))
  (assert
    (equal?
      (append odds odds)
      (my-append odds odds))))

(define (last-pair l)
  (if (null? l) (error))
  (let ((cdr-l (cdr l)))
    (if (null? cdr-l)
      (car l)
      (last-pair cdr-l))))

(define (my-reverse l)
  (define (iter l result)
    (if (null? l)
      result
      (iter (cdr l) (cons (car l) result))))
  (iter l '()))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define (same-parity f . l)
  (define (iter l)
    (if (null? l)
      '()
      (let ((car-l (car l)))
        (if (even? (+ f car-l))
          (cons car-l (iter (cdr l)))
          (iter (cdr l))))))
  (cons f (iter l)))

(define (my-map proc items)
  (if (null? items)
    '()
    (cons (proc (car items))
          (my-map proc (cdr items)))))

(define (square x)
  (* x x))

(define (square-list1 items)
  (if (null? items)
    '()
    (cons (square (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  (map square items))

(define (my-for-each proc items)
  (if (null? items)
    #t
    (my-for-each proc (cdr items))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else  (+ (count-leaves (car x))
                  (count-leaves (cdr x))))))

(define ex-2-24-1
 (car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))))

(define ex-2-24-2
  (car (car (list (list 7)))))

(define ex-2-24-3
  (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr
    (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))))))))))

(define (deep-reverse l)
  (define (iter l result)
    (cond ((null? l) result)
          ((not (pair? l)) l)
          (else (iter (cdr l)
                      (cons (iter (car l) '())
                            result)))))
  (iter l '()))

(define (fringe l)
  (define (iter l result)
    (cond
      ((null? l) result)
      ((pair? l) (iter (cdr l) (iter (car l) result)))
      (else (cons l result))))
  (reverse (iter l '())))

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cdr m))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cdr b))

(define (total-weight m)
  (define (iter x result)
    (if (pair? x)
      (iter (branch-structure (right-branch x))
            (iter (branch-structure (left-branch x)) result))
      (+ result x)))
  (iter m 0))

(define hoge-mobile
  (make-mobile
    (make-branch 2
                 (make-mobile
                   (make-branch 3 4)
                   (make-branch 5 3)))
    (make-branch 8 3)))

(define balanced-mobile
  (make-mobile
    (make-branch 4
                 (make-mobile
                   (make-branch 3 4)
                   (make-branch 6 2)))
    (make-branch 8 3)))

(define (mobile-balanced? m)
  (define (iter x)
    (if (pair? x)
      (let ((left (left-branch x))
            (right (right-branch x)))
        (let ((left-results (iter (branch-structure left)))
              (right-results (iter (branch-structure right))))
          (let ((left-weight (cdr left-results))
                (right-weight (cdr right-results)))
            (if (and (car left-results)
                     (car right-results)
                     (= (* (branch-length left) left-weight)
                        (* (branch-length right) right-weight)))
              (cons #t (+ left-weight right-weight))
              (cons #f 0)))))
      (cons #t x)))
  (car (iter m)))

(define (square-tree x)
  (if (null? x)
    '()
  (if (pair? x)
    (cons (square-tree (car x)) (square-tree (cdr x)))
    (square x))))

(define (test-square-tree)
  (assert
    (equal? (square-tree (list 1
                               (list 2 (list 3 4) 5)
                               (list 6 7)))
            (list 1 (list 4 (list 9 16) 25) (list 36 49)))))

(define (tree-map proc tree)
  (if (null? tree)
    '()
  (if (pair? tree)
    (cons (square-tree (car tree)) (square-tree (cdr tree)))
    (proc tree))))

(define (test-tree-map)
  (define tree
    (list 1
          (list 2 (list 3 4) 5)
          (list 6 7)))
  (assert (equal? (square-tree tree)
                  (tree-map square tree))))

(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map
                     (lambda (rest) (cons (car s) rest))
                     rest)))))
; 部分適用使いたいっすね

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (map-accum p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (test-map-accum)
  (assert (equal?
            (map-accum square (list 1 2 3 4))
            (list 1 4 9 16))))

(define (append-accum seq1 seq2)
  (accumulate cons seq2 seq1))

(define (test-append-accum)
  (assert (equal?
            (append-accum (list 1 2) (list 3 4))
            (list 1 2 3 4))))

(define (length-accum sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define (test-length-accum)
  (assert (=
            (length-accum (list 1 2 3 4 5 6))
            6)))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

(define (test-horner-eval)
  (assert
    (=
      (horner-eval 2 (list 1 3 0 5 0 1))
      79)))

(define (count-leaves-accum t)
  (accumulate (lambda (x y) (+ x y))
              0
              (map (lambda (x)
                     (if (pair? x)
                       (count-leaves-accum x)
                       1))
                   t)))

(define (test-count-leaves-accum)
  (count-leaves-accum (list 1 (list 2 3 (list 5)) 6)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
             '()
             (cons (accumulate op init (map (lambda (x) (car x)) seqs))
                   (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(define (test-accumulate-n)
  (define l
    (list
      (list 1 2 3)
      (list 4 5 6)
      (list 7 8 9)
      (list 10 11 12)))
  (assert
    (equal?
      (accumulate-n + 0 l)
      (list 22 26 30))))

(define m1
  (list
    (list 1 2 3 4)
    (list 4 5 6 6)
    (list 6 7 8 9)
    (list 9 8 6 2)))

(define m2
  (list
    (list 1 2 3 4)
    (list 4 5 6 6)
    (list 6 7 8 9)
    (list 1 2 3 4)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w)
         (dot-product v w))
       m))

(define (transpose mat)
  (accumulate-n (lambda (v result)
                  (cons v result))
                '()
                mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v)
           (matrix-*-vector cols v))
         m)))
