#lang racket

; downseries
; takes 3 numbers: step, high, low
; produces list of numbers from high to low (including high and possibly low) separated by step in descending order
; ex. (downseries 3 12 3) = '(12 9 6 3)
; ex. (downseries 3 8 3) = '(8 5)
; ex. (downseries 1 2 3) = '()

(define (downseries step high low)
  (if (< high low)
     '()
     (cons high (downseries step (- high step) low))))

; meow-string-map
; takes a list of strings (lst) and returns list of strings where meow has been appended to end
; ex. (meow-string-map '("hi")) = '("himeow")

(define (meow-string-map lst)
  (map (λ (string)
         (string-append string "meow")) lst))

; list-ref-div
; takes list lst and number n
; if n is negative, terminate with (error "list-ref-div: negative number")
; otherwise return ith element of list (start from 0) and i = n/(lst length)
; ex. (list-ref-div (list "a" "b" "c" d") 6) = "b"

(define (list-ref-div lst n)
  (if (< n 0)
      (error "list-ref-div: negative number")
      (list-ref lst (quotient n (length lst)))))

; next-k-items
; takes a stream s and a number k
; returns list consisting of next k elements extracted from stream

(define (next-k-items s k)
  (if (= k 0)
      '()
      (letrec ([f (λ (s) ; function that takes a stream
                (let ([pr (s)]) ; pr = result from executing stream
                  (cons (car pr) (next-k-items (cdr pr) (sub1 k)))))]) ; add
        (f s))))

; kth-item
; takes stream s and number k
; returns result of extracting k items from s but only returns the last element

(define (kth-item s k)
  (letrec ([f (λ (s) ; function that takes a stream
                (let ([pr (s)]) ; pr stores the result from executing stream once (answer, rest of stream)
                  (if (= k 1)
                      (car pr) ; return the answer (element)
                      (kth-item (cdr pr) (sub1 k)))))]) ; otherwise call kth-item with rest of stream and k-1
    (f s))) ; start at initial values

; negate-3-and-7
; is a stream of natural numbers but numbers divisible by 3 or 7 are negated

(define (negate-3-and-7)
  (letrec ([f (λ (x)
                (cond
                  [(= (modulo x 3) 0) (cons (* x -1) (λ () (f (add1 x))))] ; if divisible by 3, negate x
                  [(= (modulo x 7) 0) (cons (* x -1) (λ () (f (add1 x))))] ; if divisible by 7, negate x
                  [else (cons x (λ () (f (add1 x))))]))]) ; else just keep the same
    (f 1))) ; start at 1

; key-heart-star
; elements of stream alternate between key, heart, yellow-star
; thunk that when called produces a (heart, thunk) call-> (key, thunk) -> (yellow-star, thunk) after yellow-star cycle back
(require 2htdp/planetcute)
(define (key-heart-star)
  (letrec ([f (λ (num)
                (cond
                  [(= num 1) (cons key (λ () (f (add1 num))))] ; if num equals 1, cons a heart to a thunk
                  [(= num 2) (cons heart (λ () (f (add1 num))))] ; if num equals 2, cons a key to a thunk
                  [(= num 3) (cons yellow-star (λ () (f (- num 2))))]))]) ; else, cons a yellow-star to thunk and restart cycle
    (f 1)))
             
; two-pairs-stream
; takes a stream s and returns new stream such that each element in stream is pair '(2 . k) where k = kth s element
; ex. (next-k-items (two-pairs-stream nats) 3) = '((2 . 1) (2 . 2) (2 . 3))

(define (two-pairs-stream s)
  (letrec ([f (λ (stream)
                (let ([pr (stream)]) ; pr = call from stream
                  (let ([el (cons 2 (car pr))]) ; el = 2 consed to car of pr
                    (cons el (λ () (f (cdr pr)))))))]) ; cons pair to thunk
    (λ () (f s)))) ; thunk/function

; spin-stream
; takes two lists xs ys and returns stream that returns pairs of elements from each list and rotates through them
; ex. '(1 2 3), '("a" "b") = (1 . "a"), (2 . "b") (3 . "a") (1 . "b") (2 . "a") (3 . "b")
; hint = similar to list-ref-div but with modulus not quotient, use define, and recursive helper that takes n and adds1 inside thunk

(define (spin-stream xs ys)
  (begin
    (let ([lenx (length xs)] [leny (length ys)])
      (define (helper n)
      (cons (cons (list-ref xs (modulo n lenx)) (list-ref ys (modulo n leny))) (λ () (helper (add1 n)))))
    (λ () (helper 0)))))

; kvpv-lookup
; takes two arguments (value v and vector vec)
; returns #f if no vector element with the car equal to v, otherwise return first pair found
; not all elements have to be pairs
; works like assoc

(define (kvpv-lookup v vec)
  (letrec ([f (λ (index) ; define f helper function
                (if (> (vector-length vec) index) ; checks to see if index is within vector
                    (let ([pr (vector-ref vec index)]) ; pr = pair at index
                      (if (pair? pr) ; checks to see if element is a pair
                          (if (equal? v (car pr)) ; if element is a pair, check to see if v = car pr
                              pr ; if v = car pr, return pr
                              (f (add1 index))) ; if v != car pr, move to next element in vector
                          (f (add1 index)))) ; if element is not a pair, move to next element in vector
                    #f))]) ; v was not found in vec
    (f 0)))

; cached-lookup
; takes a list of (key,value) and a number n that represents the cache size

(define (cached-lookup lst n)
  (let ([cache (make-vector n #f)] [avail-index 0]) ; creates cache and index for next available slot in cache
    (begin
      (define (f val lst)
        (let ([kvpv-res (kvpv-lookup val cache)])
          (if (false? kvpv-res) ; checks to see if value was in cache
              (let ([assoc-res (assoc val lst)])
                (if (false? assoc-res) ; if value was not in cache (checks to see if val was in list)
                    #f ; return false if value was not in list
                    (begin
                      (if (< avail-index n)
                          (begin
                            (vector-set! cache avail-index assoc-res) ; if value was in list, store it in the cache
                            (set! avail-index (add1 avail-index))) ; update available index
                          (begin
                            (vector-set! cache 0 assoc-res)
                            (set! avail-index 0)
                            avail-index))
                      (cons #f assoc-res)))) ; return (#f, assoc return)
              (cons #t kvpv-res)))) ; if in cache, return (#t, kvpv return)
      f)))
                 

; repeatit!
; (repeatit! e1 until e2)
; evaluates e1 at least once
; evaluates e2 after each execution of e1
; it continues to evaluate e1 repeatedly while e2 returns #f, otherwise it completes and returns

(define-syntax repeatit!
  (syntax-rules (until) ; until is new keyword
    [(repeatit! e1 until e2) ; syntax
       (letrec ([f (λ ()
                     (if e2 ; if e2 is true
                         (begin (void)) ; return
                         (begin ; otherwise, evaluate e1 and e2 and call f again
                           e1
                           e2
                           (f))))])
         (f))]))

; achievements

; for what?
; for loop macro (for/count x1 e1 e2 e3 e4)
; initialize x1 to e1
; if e2 is true, evaluate e4
; finally evaluate e3
; then loop back and evaluate e2
; ex. (for/count x 0 (< x 10) (set! x (add1 x)) (print "hi")) will print "hi" 10 times.

(define-syntax for/count
  (syntax-rules ()
    [(for/count x1 e1 e2 e3 e4) ; syntax
     (let* ([x1 e1]) ; initialize x1 with e1
       (letrec ([f (λ ()
                   (if e2
                       (begin
                         e4
                         e3
                         (f))
                       (begin (void))))])
       (f)))]))
