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

(define (ex-2-53)
  (display '(list 'a 'b 'c))
  (newline)
  (display (list 'a 'b 'c))
  (newline)

  (newline)
  (display '(list (list 'george)))
  (newline)
  (display (list (list 'george)))
  (newline)

  (newline)
  (display '(cdr '((x1 x2) (y1 y2))))
  (newline)
  (display (cdr '((x1 x2) (y1 y2))))
  (newline)

  (newline)
  (display '(cadr '((x1 x2) (y1 y2))))
  (newline)
  (display (cadr '((x1 x2) (y1 y2))))
  (newline)

  (newline)
  (display '(pair? (car '(a short list))))
  (newline)
  (display (pair? (car '(a short list))))
  (newline)

  (newline)
  (display '(my-memq 'red '((red shoes) (blue socks))))
  (newline)
  (display (my-memq 'red '((red shoes) (blue socks))))
  (newline)

  (newline)
  (display '(my-memq 'red '(red shoes blue socks)))
  (newline)
  (display (my-memq 'red '(red shoes blue socks)))
  (newline))
