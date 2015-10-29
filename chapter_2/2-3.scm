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
                   (my-deriv (my-augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (my-deriv (my-multiplicand exp) var))
           (make-product (my-deriv (multiplier exp) var)
                         (my-multiplicand exp))))
        ((exponentiation? exp)
         (let ((base-exp (base exp))
               (exponent-exp (exponent exp)))
           (make-product
             (make-product
               exponent-exp
               (make-exponentiation
                 base-exp
                 (make-sum exponent-exp -1)))
             (my-deriv base-exp var))))
        (else
          (error "unknowwn expression type -- DERIV" exp))))

(define (test-my-deriv)
  (assert-equal
    '(+ (* x y) (* y (+ x 3)))
    (my-deriv '(* (* x y) (+ x 3)) 'x)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation b e)
  (cond
    ((=number? e 0) 1)
    ((=number? e 1) b)
    ((and (number? b) (number? e)) (expt b e))
    (else (list '** b e))))

(define (test-ex-2-56)
  (assert-equal
    '(+ (* 3 (** x 2)) (* 2 x))
    (my-deriv '(+ (** x 3) (** x 2)) 'x)))

(define (test-ex-2-57)
  (assert-equal
    '(+ (* x y) (* y (+ x 3)))
    (my-deriv '(* x y (+ x 3)) 'x)))

(define (my-augend s)
  (if (null? (cdddr s))
    (caddr s)
    (append '(+) (cddr s))))

(define (my-multiplicand p)
  (if (null? (cdddr p))
    (caddr p)
    (append '(*) (cddr p))))

(define (ex-2-58 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((ex-2-58-sum? exp)
         (make-sum (ex-2-58 (ex-2-58-addend exp) var)
                   (ex-2-58 (ex-2-58-augend exp) var)))
        ((ex-2-58-product? exp)
         (make-sum
           (make-product (ex-2-58-multiplier exp)
                         (ex-2-58 (ex-2-58-multiplicand exp) var))
           (make-product (ex-2-58 (ex-2-58-multiplier exp) var)
                         (ex-2-58-multiplicand exp))))
        (else
          (error "unknowwn expression type -- DERIV" exp))))

(define (ex-2-58-sum? exp)
  (and (pair? exp) (pair? (memq '+ exp))))

(define (ex-2-58-addend exp)
  (let ((a (car (split-by exp '+))))
    (if (null? (cdr a))
      (car a)
      a)))

(define (ex-2-58-augend exp)
  (let ((a (cadr (split-by exp '+))))
    (if (null? (cdr a))
      (car a)
      a)))

(define (ex-2-58-product? exp)
  (and (pair? exp) (not (pair? (memq '+ exp))) (pair? (memq '* exp))))

(define (ex-2-58-multiplier exp)
  (let ((m (car (split-by exp '*))))
    (if (null? (cdr m))
      (car m)
      m)))

(define (ex-2-58-multiplicand exp)
  (let ((m (cadr (split-by exp '*))))
    (if (null? (cdr m))
      (car m)
      m)))

(define (test-ex-2-58-a)
  (assert-equal
    4
    (ex-2-58 '(x + (3 * (x + (y + 2)))) 'x)))

(define (split-by l x)
  (if (eq? (car l) x)
    (list '() (cdr l))
    (let ((next (split-by (cdr l) x)))
      (list (append (list (car l)) (car next)) (cadr next)))))

(define (test-ex-2-58-b)
  (assert-equal
    4
    (ex-2-58 '(x + 3 * (x + y + 2)) 'x)))

(define (my-element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (my-element-of-set? x (cdr set)))))

(define (my-adjoin-set x set)
  (if (my-element-of-set? x set)
    set
    (cons x set)))

(define (my-intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((my-element-of-set? (car set1) set2)
         (cons (car set1)
               (my-intersection-set (cdr set1) set2)))
        (else (my-intersection-set (cdr set1) set2))))

(define (my-union-set set1 set2)
  (if (null? set2)
    set1
    (my-union-set (my-adjoin-set (car set2) set1) (cdr set2))))

(define (test-my-union-set)
  (assert-equal
    (list 1 2 3 4 5 6 7)
    (my-union-set (list 1 2 3 4 5 6 7) (list 1 2 3 4))))

(define (ex-2-60-element-of-set? x set)
  (pair? (memq x set)))

(define (ex-2-60-adjoin-set x set)
  (cons x set))

(define (ex-2-60-intersection-set set1 set2)
  (define (iter result set)
    (cond 
      ((null? set) result)
      ((ex-2-60-element-of-set? (car set) set1)
       (ex-2-60-intersection-set (cons (car set) result) (cdr set)))
      (else (ex-2-60-intersection-set result (cdr set)))))
  (iter '() set2))

(define (ex-2-60-union-set set1 set2)
  (append set1 set2))

; 要素の追加が頻繁に起こる集合については、ex-2-60の方式の方が計算量が少なくなる。


(define (element-of-ordered-set? x set)
  (cond ((null? set) #f)
        ((= x(car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-ordered-set? x (cdr set)))))

(define (intersection-ordered-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1 (intersection-ordered-set (cdr set1)
                                                (cdr set2))))
            ((< x1 x2)
             (intersection-ordered-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-ordered-set set1 (cdr set2)))))))

(define (adjoin-ordered-set x set)
  (define (iter past rest)
    (if (null? rest)
      set
      (let ((car-rest (car rest)))
        (cond
          ((= x car-rest) set)
          ((< x car-rest) (append (reverse past) (cons x rest)))
          (else (iter (cons car-rest past) (cdr rest)))))))
  (iter '() set))

(define (test-adjoin-ordered-set)
  (assert-equal
    (list 1 2 3 4 5)
    (adjoin-ordered-set 3 (list 1 2 4 5)))
  (assert-equal
    (list 1 2 3 4 5)
    (adjoin-ordered-set 3 (list 1 2 3 4 5))))

(define (union-ordered-set set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    (else
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (union-ordered-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (cons x1 (union-ordered-set (cdr set1) set2)))
              (else
                (cons x2 (union-ordered-set set1 (cdr set2)))))))))

(define (test-union-ordered-set)
  (assert-equal
    (list 1 2 3 4 5)
    (union-ordered-set (list 1 3 5) (list 2 3 4))))

(define (entry tree) (car tree))

(define (my-left-branch tree) (cadr tree))

(define (my-right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-tree-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-tree-set? x (my-left-branch set)))
        ((> x (entry set))
         (element-of-tree-set? x (my-right-branch set)))))

(define (adjoin-tree-set x set)
  (cond ((null? set) make-tree x '() '())
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-tree-set x (my-left-branch set))
                    (my-right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (my-left-branch set)
                    (adjoin-tree-set x (my-right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (my-left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (my-right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (my-left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (my-right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

; ex-2-63-a
; tree->list-1, tree->list-2とも要素を昇順に並べた同一のリストを作る。

; ex-2-63-b
; tree->list-2はO(n)で増加するが、tree->list-1はapendを使っているためO(nlogn)で増加する
; ↑の認識であってるかどうか検索したら、反復的プロセスか再帰的プロセスかに言及しているものがあったが、どちらもステップ数の増加という観点でいうと同じであるように思える(たぶん)
; tree->list-1, tree->list-2とも要素を昇順に並べた同一のリストを作る。
