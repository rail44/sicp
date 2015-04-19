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
