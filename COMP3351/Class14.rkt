#lang racket

; defining the stream nats
(define (nats) ; 0 arguments
  (define (f x) (cons x (λ () (f (add1 x))))) ; function with one argument that returns cons cell (pair), x cons with thunk that adds one to x
  (f 1)) ; initalize function call with one

; macro examples
#|(my-if e1 then e2 else e3) => (if e1 e2 e3)
(comment-out e1 e2) => e2
(my-delay e) -> (λ () e)|#

(let ([hd 0] [cr 1]) hd)

(define-syntax my-if ; my-if is macro name
  (syntax-rules (then else) ; then else is syntax that macro owns (new words in language)
    [(my-if e1 then e2 else e3) (if e1 e2 e2)] ; left side is now the form you use
    [(my-if e1 then e2) (when e1 e2)]))

(define-syntax comment-out
  (syntax-rules ()
    [(comment-out e1 e2) e2]))

; delays execution until needed
(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e) (mcons #f (λ () e))])) ; thunk created

; would be called in my-delay
(define (my-force1 pr)
  (cond
    [(mcar pr) (mcdr pr)]
    [else
     (set-mcar! pr #t) ; sets first element to true
     (set-mcdr! pr ((mcdr pr))) ; sets second element (rest) to the rest of pr
     (mcdr pr)])) ; returns rest of pr

(define-syntax my-force2
  (syntax-rules ()
    [(my-force e)
     (let ([x e])
       (if (mcar x)
           (mcdr x)
           (begin ; evaluates everything and returns last thing done
             (set-mcar! x #t)
             (set-mcdr! x ((mcdr x)))
             (mcdr x))))]))

; one version of double
(define-syntax double1
  (syntax-rules ()
    [(double1 e) (* 2 e)]))

; another version of double
(define-syntax double2
  (syntax-rules ()
    [(double2 e) (+ e e)]))

; third version of double
(define-syntax double3
  (syntax-rules ()
    [(double3 e)
     (let ([x e])
       (+ x x))]))


(define-syntax double4
  (syntax-rules ()
    [(double4 e)
     (let* ([zero 0] ; silly because we dont need a binding to 0
            [x e])
       (+ x x zero))]))
           
(define-syntax let2
  (syntax-rules ()
    ; case without any arguments to let2
    [(let2 () body) body]
    ; case with one argument
    [(let2 (var val) body)
     (let ([var val]) body)]
    ; case with two arguments
    [(let2 (var1 val1 var2 val2) body)
     (let* ([var1 val1]
            [var2 val2])
       body)]))

(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () body) body] ; no arguments
    [(my-let* ([var0 val0]
               [var-rest val-rest] ...)
              body)
     (let ([var0 val0])
       (my-let* ([var-rest val-rest] ...)
                body))]))

(define-syntax for
  (syntax-rules (to do)
    [(for low to hi do body)
     (let ([l low]
           [h hi])
       (letrec ([loop (λ (it)
                        (cond
                          [(> it h) (begin #t (void))] ; if it is higher than high, do nothing
                          [else (begin ; if it is lower than high
                                  body ; do the body
                                  (loop (add1 it)))]))]) ; evaluate loop(it+1)
       (loop l)))]))
           
                 






 