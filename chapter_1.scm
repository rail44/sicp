(define (square x) (* x x))

(define (sum-of-squares a b)
  (+ (square a)
     (square b)))

(define (ex-1-3 a b c)
  (if (> a b)
    (if (> b c)
      (sum-of-squares a b)
      (sum-of-squares a c))
    (if (> a c)
      (sum-of-squares a b)
      (sum-of-squares b c))))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess(/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) (abs (* x 0.00000001))))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x) x)))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))

(define (cbrt x)
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (define (good-enough? guess)
    (< (abs (- (improve guess) guess)) (abs (* x 0.00000001))))

  (define (iter guess)
    (if (good-enough? guess)
      guess
      (iter (improve guess))))

  (iter 1))

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1 ) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(define (ex-1-11-rec n)
  (if (< n 3)
    n
    (+ (ex-1-11-rec (- n 1)) (* 2 (ex-1-11-rec (- n 2))) (* 3 (ex-1-11-rec (- n 3))))))

(define (ex-1-11-iter n)
  (define (iter a b c count)
    (if (= count 0)
      a
      (iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

  (if (< n 3)
    n
    (iter 2 1 0 (- n 2))))

(define (pascal l n)
  (if (= l 1)
    (if (= n 1) 1 0)
    (+ (pascal (- l 1) (- n 1)) (pascal (- l 1) n))))

