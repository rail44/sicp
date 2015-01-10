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
