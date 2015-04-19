(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< (* n d) 0)
      (cons (/ (abs n) g -1) (/ (abs d) g))
      (cons (/ (abs n) g) (/ (abs d) g)))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (define (average x y)
    (/ (+ x y) 2))
  (let ((start-p (start-segment s))
        (end-p (end-segment s)))
    (make-point (average (x-point start-p)
                         (x-point end-p))
                (average (y-point start-p)
                         (y-point end-p)))))

(define (make-rectangle p1 p2)
  (cons p1 p2))

(define (p1-rectangle r)
  (car r))

(define (p2-rectangle r)
  (cdr r))

(define (width-rectangle r)
  (abs (- (x-point (p1-rectangle r))
          (x-point (p2-rectangle r)))))

(define (height-rectangle r)
  (abs (- (y-point (p1-rectangle r))
          (y-point (p2-rectangle r)))))

(define (perimeter-rectangle r)
  (+ (* (width-rectangle r) 2)
     (* (height-rectangle r) 2)))

(define (area-rectangle r)
  (* (width-rectangle r) (height-rectangle r)))

(define (test-rectangle)
  (let ((p1 (make-point 1 3))
        (p2 (make-point 3 -1)))
    (let ((r (make-rectangle p1 p2)))
      (assert
        (= (area-rectangle r)
           8))
      (assert
        (= (perimeter-rectangle r)
           12)))))

(define (make-rectangle-by-size base width height)
  (cons base
        (make-point
          (+ (x-point base) width)
          (+ (y-point base) height))))

(define (test-rectangle-by-size)
  (let ((base (make-point 1 3))
        (w 5)
        (h 12))
    (let ((r (make-rectangle-by-size base w h)))
      (assert
        (= (area-rectangle r)
           60))
      (assert
        (= (perimeter-rectangle r)
           34)))))

(define (ex-2-4-cdr z)
  (z (lambda (p q) q)))

(define (test-ex-2-4)
  (define (cons x y)
    (lambda (m) (m x y)))
  (assert
    (=
      (ex-2-4-cdr (cons 3 4))
      4)))

(define (ex-2-5-cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (ex-2-5-iter z n i)
  (if (= (remainder z n) 0)
    (ex-2-5-iter (/ z n) n (+ i 1))
    i))

(define (ex-2-5-car z)
  (ex-2-5-iter z 2 0))

(define (ex-2-5-cdr z)
  (ex-2-5-iter z 3 0))

(define (test-ex-2-5)
  (let ((pair (ex-2-5-cons 11 13)))
    (assert
      (=
        (ex-2-5-car pair)
        11))
    (assert
      (=
        (ex-2-5-cdr pair)
        13))))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (ex-2-6-plus a b)
  (lambda (f) (lambda (x) ((b f) ((a f) x)))))

(define (test-ex-2-6)
  (assert
    (= (((ex-2-6-plus one two) (lambda (x) (+ x 1))) 0)
       3)))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((c1 (< (* (upper-bound x) (lower-bound x)) 0))
        (c2 (< (* (upper-bound y) (lower-bound y)) 0)))
    (cond ((and c1 c2)
           (let ((p1 (* (lower-bound x) (lower-bound y)))
                 (p2 (* (lower-bound x) (upper-bound y)))
                 (p3 (* (upper-bound x) (lower-bound y)))
                 (p4 (* (upper-bound x) (upper-bound y))))
             (make-interval (min p1 p4)
                            (max p2 p3))))
          (c1
            (make-interval (* (lower-bound x) (upper-bound y))
                           (* (upper-bound x) (upper-bound y))))
          (c2
            (make-interval (* (upper-bound x) (lower-bound y))
                           (* (upper-bound x) (upper-bound y))))
          (else
            (make-interval (* (lower-bound x) (lower-bound y))
                           (* (upper-bound x) (upper-bound y)))))))

(define (div-interval x y)
  (if (< (* (upper-bound y) (lower-bound y)) 0)
    (error)
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))

(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (print-interval i)
  (display (lower-bound i))
  (display ":")
  (display (upper-bound i))
  (newline))

(define (make-random-interval maximum)
  (let ((upper (random  maximum)))
    (make-interval (- upper (random maximum)) upper)))

(define (test-ex-2-9)
  (let ((a (make-random-interval 20))
        (b (make-random-interval 20)))
    (define sub (sub-interval a b))
    (define add (add-interval a b))
    (display "a: ")
    (print-interval a)
    (display "b: ")
    (print-interval b)
    (assert
      (= (width-interval sub)
         (+ (width-interval a) (width-interval b))))
    (assert
      (= (width-interval add)
         (+ (width-interval a) (width-interval b))))))

(define (make-interval-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center-interval i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-interval-center-percent c p)
  (make-interval-center-width (c (/ (* c p) 100))))

(define (percent-interval i)
  (* (/ (width-interval i) (center-interval i)) 100))

(define (ex-2-13 x y)
  (let ((center (* (center-interval x) (center-interval y)))
        (percent (+ (percent-interval x) (percent-interval y))))
    (make-interval-center-percent center percent)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
