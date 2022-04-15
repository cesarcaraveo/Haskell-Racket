#lang racket

; building lexer
; lexer takes a file and produces stream of tokens

; tool to help us make lexer
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)) ; prefix functions with :

; by deafult in Racket, all functions in module are hidden
; this exposes them all
(provide (all-defined-out))

(define-tokens names-and-values (NUMBER IDENTIFIER)) ; num and id are token names (don't carry anything)

(define-empty-tokens keywords (ADD
                               SUB
                               MULT
                               DIV
                               LPAREN
                               RPAREN
                               EOF)) ; tokens that don't carry anything

(define exprlexer
  (lexer-src-pos ; includes line # and col # with token in case of error
   ["+" (token-ADD)] ; addition, left side is expression we match, right side is action we take (want tokens)
   ["-" (token-SUB)]
   ["*" (token-MULT)]
   ["/" (token-DIV)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ; lexeme is the currently matched string by the lexer
   [(:+ numeric) (token-NUMBER (string->number lexeme))] ; numeric came from lexer racket doc
   [(:+ alphabetic) (token-IDENTIFIER (string->symbol lexeme))]
   ; to deal with whitespace
   [whitespace (return-without-pos (exprlexer input-port))]
   [(eof) (token-EOF)])) ; end of file

; used to get token (turn into token?)
(define (get-tokenizer in)
  (λ () (exprlexer in)))

(define (lex in) ; brings input port 
  (port-count-lines! in) ; counts lines (used for errors)
  (let ([tokenize (get-tokenizer in)]) ; thunk on lexer
    (define (lexfun) ; lex function to call repeatedly to get stream of tokens
      (let ([tok (tokenize)]) ; pulls first token out of input port
        (cond
          [(eq? (position-token-token tok) (token-EOF)) null] ; if EOF, then null (to terminate list)
          [else (cons (position-token-token tok) (lexfun))]))) ; if other token, cons token to rest of stream?
    (lexfun)))

(define (lexstr str)
  (lex (open-input-string str)))

; building parser  (want this in a different file)

(require parser-tools/yacc ; yacc
         (prefix-in lex: parser-tools/lex)) ; prefix everything with lex: -- include file name as well where lexer is
(provide (all-defined-out))

; define our AST structures
(struct var-name (sym) #:transparent)
(struct number (num) #:transparent)
(struct add-op (left right) #:transparent)
(struct sub-op (left right) #:transparent)
(struct mult-op (left right) #:transparent)
(struct div-op (left right) #:transparent)

(define expr-parser
  (parser ; command that came from yacc tools
   (src-pos) ; source position is included in lexer so need to include this
   (start expr)
   (end EOF) ; end here
   (tokens names-and-values keywords) ; what kinds of tokens we have
   (error (λ (tok-ok? tok-name tok-value start-pos end-pos) ; what to do whenever theres an error
            (printf "Parsing error at line ~a, col ~a, token ~a, value ~a, tok-ok? ~a~n"
                    (lex:position-line start-pos)
                    (lex:position-col start-pos)
                    tok-name
                    tok-value
                    tok-ok?)))    
   (grammar ; define grammar in language
    (expr
     [(LPAREN expr RPAREN) $2]; left side says rule, right side is what we do ($2 match second thing (expr))
     [(expr ADD expr) (add-op $1 $3)] ; calls add-op struct with first and third elements (expressions)
     [(expr SUB expr) (sub-op $1 $3)]
     [(expr MULT expr) (mult-op $1 $3)]
     [(expr DIV expr) (div-op $1 $3)]
     [(NUMBER) $1]
     [(IDENTIFIER) $1]))))

; parse from any input port
(define (parse in)
  (port-count-lines! in)
  (expr-parser (get-tokenizer in)))

; parse strings 
(define (parse-str str)
  (let ([in (open-input-string str)])
    (parse in)))
   










   
