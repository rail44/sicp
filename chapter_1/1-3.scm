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

(assert (=
  ((lambda (x)
      (define (term x) x)
      (define (next x) (+ x 1))
      (ex-1-31-b term 2 next x))
    6)
  720))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (ex-1-32-sum term a next b)
  (define (combiner a b) (+ a b))
  (accumulate combiner 0 term a next b))

(assert (=
          (sum cube 1 inc 10)
          (ex-1-32-sum cube 1 inc 10)))

(define (ex-1-32-product term a next b)
  (define (combiner a b) (* a b))
  (accumulate combiner 1 term a next b))

(assert (=
          (product cube 1 inc 5)
          (ex-1-32-product cube 1 inc 5)))

(define (ex-1-32-b combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (ex-1-32-b combiner null-value term (next a) next b))))

(define (ex-1-32-b-sum term a next b)
  (define (combiner a b) (+ a b))
  (ex-1-32-b combiner 0 term a next b))

(assert (=
          (ex-1-32-sum cube 1 inc 10)
          (ex-1-32-b-sum cube 1 inc 10)))

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

(assert (=
          (ex-1-33-a 1 11)
          208))

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

(assert 
  (let ((nd (lambda (i) 1.0)))
    (=
      (cont-frac-recur nd nd 100)
      (cont-frac-iter nd nd 100))))
