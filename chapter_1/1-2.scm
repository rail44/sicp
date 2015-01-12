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

(define (fast-expt b n)
  (define (even? n)
    (= (remainder n 2) 0))
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter b n)
  (define (even? n)
    (= (remainder n 2) 0))

  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* a b)))))

  (iter b n 1))

(define (ex-1-17 a b)
  (define (double n) (* 2 n))
  (define (halve n) (/ n 2))
  (define (even? n)
    (= (remainder n 2) 0))

  (define (proc a b)
    (if (= b 0)
      0
      (if (even? b)
	(double (* a (halve b)))
	(+ a (* a (- b 1))))))
  (proc a b))

(define (ex-1-18 a b)
  (define (double n) (* 2 n))
  (define (halve n) (/ n 2))
  (define (even? n)
    (= (remainder n 2) 0))

  (define (iter a b c)
    (cond ((= b 0) c)
          ((even? b) (iter (double a) (halve b) c))
          (else (iter a (- b 1) (+ a c)))))

  (iter a b 0))
