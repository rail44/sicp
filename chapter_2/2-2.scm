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
