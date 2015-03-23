(load "1-1")
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
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* a b)))))

  (iter b n 1))

(define (ex-1-17 a b)
  (define (double n) (* 2 n))
  (define (halve n) (/ n 2))

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

  (define (iter a b c)
    (cond ((= b 0) c)
          ((even? b) (iter (double a) (halve b) c))
          (else (iter a (- b 1) (+ a c)))))

  (iter a b 0))

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if ( = count 0)
    b
    (fib-iter (+ a b) a (- count 1 ))))

(define (ex-1-19 n)
  (define (fib n)
    (fib-iter 1 0 0 1 n))

  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))
                     (+ (* 2 q p) (square q))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (> n 1)
    (= n (smallest-divisor n))
    #f))

(define (ex-1-22 a b)
  (define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (current-milliseconds)))

  (define (start-prime-test n start-time)
    (if (prime? n)
      (report-prime (- (current-milliseconds) start-time))))

  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

  (define (serch-for-primes a b)
    (timed-prime-test a)
    (if (< a b)
      (serch-for-primes (+ a 1) b)))

  (serch-for-primes a b))

(define (ex-1-24 n)
  (define (smallest-divisor n)
    (find-divisor n 2))

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  
  (define (prime? n)
    (= n (smallest-divisor n)))

  (define (next n)
    (if (= n 2)
      3
      (+ n 1)))

  (define (start n)
    (end n (current-milliseconds)))

  (define (end n before)
    (prime? n)
    (display (- (current-milliseconds) before)))

  (start n))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (ex-1-24 n)
  (define (timed-prime-test n)
    (display n)
    (start-prime-test n (current-milliseconds))
    (newline))

  (define (start-prime-test n start-time)
    (if (fast-prime? n 1)
      (report-prime (- (current-milliseconds) start-time))))

  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

  (timed-prime-test n))

(define (ex-1-25 base exp m)
  (remainder (fast-expt base exp) m))

(define (ex-1-27 n)
  (define (iter n a)
    (cond
      ((>= a n) #t)
      ((= (expmod a n n) a) (iter n (+ a 1)))
      (else #f)))
  (iter n 1))

(define (ex-1-28 n)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((= (remainder base m) 1) 0)
          ((even? exp)
            (remainder (square (expmod base (/ exp 2) m))
                        m))
          (else
            (remainder (* base (expmod base (- exp 1) m))
                       m))))
  (define (try-it a)
    (= (expmod a n n) a))

  (try-it (+ (random (- n 1)) 1)))
