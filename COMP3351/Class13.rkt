#lang racket

(define (mod2b x)
  (define (even? x) (if (zero? x) #t (odd? (- x 1))))
  (define (odd? x) (if (zero? x) #f (even? (sub1 x))))
  (if (even? x) 0 1))

; bad example of letrec
#|(define (bad-letrec x)
  (letrec ([x z]
           [z 13])
    (if x y z)))|#

#|(define b 3)
(define (f x) (* 1 (1 + x b)))
(define c (+ b 4))
(set! b 5) ; b = 5
(define z (f 4))
(define w c)|#

; (let ([x 5]) (set! x 6) x) returns 6
; '(1 . 5) -- pair or cons cell (cons 1 5)

(define (my-if-bad x y z) (if x y z)) ; must evaulate if e1 e2 e3 e1 first and then evaluate e2 or 3

(define (factorial-wrong x)
  (my-if-bad (= x 0)
             1
             (* x (factorial-wrong (sub1 x)))))

(define (my-if x y z) (if x (y) (z)))

(define (factorial x)
  (my-if (= x 0)
         (λ () 1)
         (λ () (* x (factorial (sub1 x))))))

; construct a delayed execution
(define (my-delay f)
  (mcons #f f))

(define (my-force th)
  (cond
    [(mcar th) (mcdr th)] ; if the car of the mcons cell is true, then return cdr
    [else
     (set-mcar! th #t) ; set car to true
     (set-mcdr! th ((mcdr th))) ; set car to mcdr th
     (mcdr th)]))

(define (my-mult x y)
  (cond
    [(= x 0) 0]
    [(= x 1) y]
    [else (+ y (my-mult (- x 1) y))]))

(define (my-mult-t x y-thunk)
  (cond
    [(= x 0) 0]
    [(= x 1) (y-thunk)]
    [else (+ (y-thunk) (my-mult-t (- x 1) (y-thunk)))]))

(define (my-mult-f e1 e2)
  (letrec ([x e1]
        [y (my-delay (λ () e2))])
    (cond
      [(= x 0) 0]
      [(= x 1) (my-force y)]
      [else (+ (my-force y) (my-mult-f (sub1 x) (my-force y)))])))

#|(define (my-mult-df x y-promise)
  (cond
    [(= x 0) 0]
    [(= x 1) (my-force y-promise)]
    [else (+ y (my-mult (- x 1) y))]))|#

; a stream of ones
(define ones (λ () (cons 1 ones)))

; a stream of natural numbers
(define nats
  (letrec ([f (λ (x) (cons x (λ () (f (add1 x)))))])
    (λ () (f 1))))

; stream of powers of 2
(define (powers-of-two)
  (letrec ([f (λ (x) (cons x (λ () (f (* x 2)))))])
    (f 2)))

; stream that alternates from 1 to -1
(define (one-or-minus-one)
  (letrec ([f (λ (x) (cons x (λ () (f (* x -1)))))])
    (f 1)))

(define (number-until stream tester)
  (letrec ([f (λ (stream ans)
             (let ([pr (stream)]) ; stores the result from exectuing stream in pr
               (if (tester (car pr)) ; if tester equals the first value of the pr
                   ans
                   (f (cdr pr) (add1 ans)))))])
    (f stream 1)))
           
(define (stream-maker fn arg)
  (letrec ([f (λ (x)
                (cons x (λ () (f (fn x arg)))))])
    (λ () (f arg))))

(define ones* (stream-maker (λ (x y) 1) 1))
(define nats* (stream-maker + 1))
(define powers-of-two* (stream-maker * 2))

(define (fibonacci x)
  (if (or (= x 1) (= x 2))
      1
      (+ (fibonacci (- x 1))
         (fibonacci (- x 2)))))

(define (fib1 x)
  (define (f acc1 acc2 y)
    (if (= y x)
        (+ acc1 acc2)
        (f (+ acc1 acc2) acc1 (+ y 1))))
  (if (or (= x 1) (= x 2))
      1
      (f 1 1 3)))

(define (fibm x)
  (define memo null)
  (define (f x)
    (let ([ans (assoc x memo)])
      (cond
        [ans (cdr ans)]
        [else
         (let ([new-ans (if (or (= x 1) (= x 2))
                            1
                            (+ (f (- x 1))
                               (f (- x 2))))])
           (set! memo (cons (cons x new-ans) memo))
           new-ans)])))
  (f x))
      
          