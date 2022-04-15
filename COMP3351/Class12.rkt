#lang racket

(define a 3) ; how to define a variable

#| multiline comment |#
; 3 + (2 * 5)
(define foo-bar (+ 3 (* 2 5))) ; prefix notation, need parantheses around everything


(define cube1
  (lambda (x) ; lambda is anonymous function ?
    (* x (* x x))))

; if (e1 e2 e3)
(define pow
  (lambda (x y)
    (if (= y 0)
        1
        (* x (pow x (- y 1))))))

; multiple arguments
(define cube2
  (lambda (x)
    (* x x x)))

; no anonymous function -- cube3 is function name and after are parameters
(define (cube3 x)
  (* x x x))

; lambda is command+\
(define cube4
  (λ (x) (* x x x)))

; nested function (currying?)
(define pow*
  (λ (x)
    (λ (y)
      (if (= y 0)
          1
          (* x ((pow* x) (- y 1)))))))

; curry function 
(define ((pow** x) y)
  (if (= y 0)
      1
      (* x ((pow** x) (- y 1)))))

; to define list: (list 1 2 3 4 5)
; or '(1 2 3 4 5)
; create list: (cons 1 null) = '(1)
; head of list: (car '(1 2 3)) = 1 or (first '(1 2 3))
; tail (cdr '(1 2 3) = '(2 3) or (rest '(1 2 3))
; second ex. : (second '(1 2 3)) = 2
; is null? = (null? '())

; sum elements in list

(define (sum xs) ; defines function 
  (if (null? xs) ; if list is null
      0 ; then 0
      (+ (first xs) (sum (rest xs))))) ; otherwise add beginning of list to the sum of the rest of the list

; append two lists
(define (append xs ys) 
  (if (null? xs) ; if xs is null
      ys ; return ys
      (cons (first xs) (append (rest xs) ys))))

; map
(define (map fun list)
  (if (null? list)
      null
      (cons (fun (first list)) (map fun (rest list)))))

; factorial recursively
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

; function with no arguments
(define (foo) "hi")

; sum a list that contains either a number or a list of numbers
(define (sumlst xs)
  (if (null? xs)
      0
      (if (number? (first xs))
          (+ (first xs) (sumlst (rest xs)))
          (+ (sumlst (first xs)) (sumlst (rest xs))))))

; conditionals
; same as code above
(define (sumcond xs)
  (cond ; different conditions
    [(null? xs) 0] ; if null list
    [(number? (first xs)) (+ (first xs) (sumcond (rest xs)))] ; if number
    [else (+ (sumcond (first xs)) (sumcond (rest xs)))])) ; else 


