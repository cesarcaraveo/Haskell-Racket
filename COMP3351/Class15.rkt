#lang racket

; implement the expression language using lists
(define (Const i) (list 'Const i)) ; back tick, comma -- it will evaluate i -- `(Const ,i)

(define (Negate e) (list 'Negate e))

(define (Add e1 e2) (list 'Add e1 e2))

(define (Multiply e1 e2) `(Multiply ,e1 ,e2))

; helper functions to test types
(define (Const? exp) (eq? (car exp) 'Const)) ; ex. (Const? (Const 5))
(define (Negate? exp) (eq? (car exp) 'Negate))
(define (Add? exp) (eq? (car exp) 'Add))
(define (Multiply? exp) (eq? (car exp) 'Multiply))

(define (Const-int exp) (car (cdr exp)))
(define (Negate-exp exp) (car (cdr exp)))
(define (Add-e1 exp) (car (cdr exp)))
(define (Add-e2 exp) (car (cdr exp)))
(define (Multiply-e1 exp) (second exp))
(define (Multiply-e2 exp) (third exp))

(define (eval-exp e)
  (cond
    [(Const? e) e]
    [(Negate? e)
     (let ([result (eval-exp (Negate-exp e))])
       (Const (- (Const-int result))))]
    [(Add? e)
     (let ([res1 (Const-int (eval-exp (Add-e1 e)))]
           [res2 (Const-int (eval-exp (Add-e2 e)))])
       (Const (+ res1 res2)))]
    [(Multiply? e)
     (let ([res1 (Const-int (eval-exp (Multiply-e1 e)))]
           [res2 (Const-int (eval-exp (Multiply-e2 e)))])
       (Const (* res1 res2)))]
    [else (error "eval-exp expected an exp")]))


; "import" = require
; test cases
(require test-engine/racket-tests)
;(check-expect (eval-exp (Multiply (Negate (Add (Const 4) (Const 2))) (Const 7)))
             ; (Const -42))

; struct named foo
(struct foo (bar baz quux) #:transparent #:mutable) ; access bar -> (foo-bar)

; define same thing using struct
(struct const (val) #:transparent) ; transparent lets you see val
(struct negate (exp) #:transparent)
(struct add (e1 e2) #:transparent)
(struct mult (e1 e2) #:transparent)
(struct bool (val) #:transparent)
(struct if-then-else (e1 e2 e3) #:transparent)
(struct eq-num (e1 e2) #:transparent)

(define (eval e)
  (cond
    [(const? e) e]
    [(negate? e)
     (let ([res (const-val (eval (negate-exp e)))])
       (const (- res)))]
    [(add? e)
     (let ([res1 (const-val (eval (add-e1 e)))]
           [res2 (const-val (eval (add-e2 e)))])
       (const (+ res1 res2)))]
    [(mult? e)
     (let ([res1 (const-val (eval (mult-e1 e)))]
           [res2 (const-val (eval (mult-e2 e)))])
       (const (* res1 res2)))]
    [else (error "You sent me bad stuff!")]))

(check-expect (eval (mult (negate (add (const 4) (const 2))) (const 7))) (const -42))

; pattern matching
(define (eval1 e)
  (match e
    ; the base cases of our evaluation
    [(const _) e]
    [(bool _ ) e]
    [(negate exp)
     (let ([res (const-val (eval1 exp))])
       (const (- res)))]
    [(add e1 e2)
     (let ([res1 (const-val (eval1 e1))]
           [res2 (const-val (eval1 e2))])
       (const (+ res1 res2)))]
    [(mult e1 e2)
     (let ([res1 (const-val (eval1 e1))]
           [res2 (const-val (eval1 e2))])
       (const (* res1 res2)))]
    [(if-then-else e1 e2 e3)
     (let ([test (eval1 e1)])
       (if (bool? test)
           (match test
             [(bool #t) (eval1 e2)]
             [_ (eval1 e3)]
           (error "if-then-else requires a bool for the test"))))]
    [(eq-num e1 e2)
     (let ([res1 (eval1 e1)]
           [res2 (eval1 e2)])
       ; this is the dynamic type checking phase
       (if (and (const? res1)
                (const? res2))
           ; this is the actual evaluation of the operation
           (bool (= (const-val res1)
                    (const-val res2)))
           (error "eq-num requires const types")))]
    [_ (error "You sent me bad stuff!")]))

(check-expect (eval1 (mult (negate (add (const 4) (const 2))) (const 7))) (const -42))
(test)


