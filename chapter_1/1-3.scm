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
