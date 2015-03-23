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
