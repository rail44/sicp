(use sicp)

(define (assert-equal a b)
  (assert (equal? a b)))

(define (my-memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (my-memq item (cdr x)))))

(define (test-my-memq)
  (assert-equal #f (my-memq 'apple '(pear banana prune)))
  (assert-equal
    '(apple pear)
    (my-memq 'apple '(x (apple sauce) y apple pear))))

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

(define (ex-2-54 a b)
  (if (and (pair? a) (pair? b))
    (and (ex-2-54 (car a) (car b))
         (ex-2-54 (cdr a) (cdr b)))
    (eq? a b)))

(define (test-ex-2-54)
  (assert-equal #t (ex-2-54 '(this is a list) `(this is a list)))
  (assert-equal #f (ex-2-54 '(this is a list) `(this (is a) list))))

; 'hoge は手続き quote の糖衣構文であるため、''hoge は (quote (quote hoge)) と解釈される。
; よって (car ''hoge) は quote を返す。

(define (my-deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (my-deriv (addend exp) var)
                   (my-deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (my-deriv (multiplicand exp) var))
           (make-product (my-deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknowwn expression type -- DERIV" exp))))

(define (test-my-deriv)
  (assert-equal
    '(+ (* x y) (* y (+ x 3)))
    (my-deriv '(* (* x y) (+ x 3)) 'x)))
