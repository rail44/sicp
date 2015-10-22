(define (my-memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (my-memq item (cdr x)))))

(define (test-my-memq)
  (assert (equal? #f (my-memq 'apple '(pear banana prune))))
  (assert
    (equal?
      '(apple pear)
      (my-memq 'apple '(x (apple sauce) y apple pear)))))
