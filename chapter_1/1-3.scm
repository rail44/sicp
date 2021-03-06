(load "1-2")

(define (cube x) (* x x x))

(define (sum-intergers a b)
  (if (> a b)
    0
    (+ a (sum-intergers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (+ a 1) b))))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (inc x)
  (+ x 1))

(define (ex-1-29 f a b n)
  (define h (/ (- b a) n))
  (define (arg n)
    (+ a (* n h)))
  (define (term n)
    (*
      (if (= (remainder n 2) 0)
        4
        2)
      (f (arg n))))
  (* (/ h 3) (+ (f (arg 0)) (sum term 1 inc (- n 1)) (f (arg n)))))

(define (ex-1-30 term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorical x)
  (define (term x) x)
  (define (next x) (+ x 1))
  (product term 2 next x))

(define (ex-1-31-pi n)
  (define (next n) (+ n 1))
  (define (mother n)
    (if (= (remainder n 2) 0)
      n
      (+ n 1)))
  (define (child n)
    (+ 1 (mother n)))
  (define (term n)
    (/ (mother (+ n 1)) (child n)))
  (* (product term 1 next n) 4))

(define (ex-1-31-b term a next b)
  (if (> a b)
    1
    (* (term a) (ex-1-31-b term (next a) next b))))

(define (test-ex-1-31-b)
  (assert (=
            ((lambda (x)
               (define (term x) x)
               (define (next x) (+ x 1))
               (ex-1-31-b term 2 next x))
             6)
            720)))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (ex-1-32-sum term a next b)
  (define (combiner a b) (+ a b))
  (accumulate combiner 0 term a next b))

(define (test-ex-1-32-sum)
  (assert (=
            (sum cube 1 inc 10)
            (ex-1-32-sum cube 1 inc 10))))

(define (ex-1-32-product term a next b)
  (define (combiner a b) (* a b))
  (accumulate combiner 1 term a next b))

(define (test-ex-1-32-product)
  (assert (=
          (product cube 1 inc 5)
          (ex-1-32-product cube 1 inc 5))))

(define (ex-1-32-b combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (ex-1-32-b combiner null-value term (next a) next b))))

(define (ex-1-32-b-sum term a next b)
  (define (combiner a b) (+ a b))
  (ex-1-32-b combiner 0 term a next b))

(define (test-ex-1-32-b-sum)
          (assert (=
                    (ex-1-32-sum cube 1 inc 10)
                    (ex-1-32-b-sum cube 1 inc 10))))

(define (filter-accumulate filter-cond combiner null-value term a next b)
  (define (iter a result)
    (cond
      ((> a b) result)
      ((filter-cond a) (iter (next a) (combiner (term a) result)))
      ((iter (next a) result))))
  (iter a null-value))

(define (ex-1-33-a a b)
  (define (filter-cond a)
    (prime? a))
  (define (combiner a b) (+ a b))
  (define (term a) (* a a))
  (define (next a) (+ a 1))
  (filter-accumulate filter-cond combiner 0 term a next b))

(define (test-ex-1-33-a)
  (assert (=
            (ex-1-33-a 1 11)
            208)))

(define (ex-1-33-b n)
  (define (filter-cond i)
    (= (gcd i n) 1))
  (define (combiner a b) (* a b))
  (define (term a) a)
  (define (next a) (+ a 1))
  (filter-accumulate filter-cond  combiner 1 term 1 next n))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (ex-1-35)
  (define (func x) (+ 1 (/ 1 x)))
  (fixed-point func 1))

(define (fixed-point-with-display f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (ex-1-36)
  (define (func x)
    (/ (log 1000) (log x)))
  (fixed-point-with-display func 2))

(define (ex-1-36-with-average)
  (define (func x)
    (/ (+ (/ (log 1000) (log x)) x) 2))
  (fixed-point-with-display func 2))

(define (cont-frac-recur n d k)
  (define (func i)
    (if (> i k)
      0
      (/ (n i) (+ (d i) (func (+ i 1))))))
  (func 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (test-cont-frac)
  (assert 
    (let ((nd (lambda (i) 1.0)))
      (=
        (cont-frac-recur nd nd 100)
        (cont-frac-iter nd nd 100)))))

(define (ex-1-38 k)
  (define (d i)
    (let ((i-plus-one (+ i 1)))
      (if (= (remainder i-plus-one 3) 0)
        (* (/ i-plus-one 3) 2)
        1)))
  (+
    (cont-frac-iter (lambda (i) 1.0)
                    d
                    k)
    2))

(define (ex-1-39 x k)
  (define (n i)
    (if (= i 1)
      x
      (* -1 x x)))
  (define (d i)
    (- (* i 2) 1))
  (cont-frac-iter n d k))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transfarm g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transfarm g) guess))

(define (sqrt-with-newtons-method x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (ex-1-36-with-newtons-method)
  (newtons-method (lambda (x) (- (expt x x) 1000))
                  1.0))

(define (fixed-point-of-ransform g transform guess)
  (fixed-point (transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(define (double f)
  (lambda (x) (f (f x))))

(define (own-compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1) (own-compose f result))))
  (iter n (lambda (x) x)))

(define (smooth f)
  (define dx 0.00001)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))))))

(define (ex-1-44 f n)
  ((repeated smooth n) f))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (ex-1-45 x n)
  (let ((average-times (floor (/ (log n) (log 2)))))
    (fixed-point ((repeated average-damp average-times) (lambda (y) (/ x (expt y (- n 1)))))
                 1.0)))

(define (interactive-improve enough? next)
  (lambda (x)
    (define (iter guess)
      (if (enough? guess)
        guess
        (iter (next guess))))
    (iter x)))

(define (ex-1-46-sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess)) (abs (* x 0.00000001))))
  ((interactive-improve good-enough?
                        improve) x))

(define (test-ex-1-46-sqrt)
  (assert (=
            (ex-1-46-sqrt 9)
            (my-sqrt 9))))

(define (ex-1-46-fixed-point f first-guess)
  (define (next guess) (f guess))
  (define (enough? guess)
    (< (abs (- guess (next guess))) tolerance))
  (next ((interactive-improve enough?
                        next) first-guess)))

(define (test-ex-1-46-fixed-point)
  (assert (=
            (fixed-point cos 1.0)
            (ex-1-46-fixed-point cos 1.0))))
