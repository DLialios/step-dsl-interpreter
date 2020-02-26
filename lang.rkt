#lang racket
(#%require (lib "eopl.ss" "eopl"))
(#%provide (all-defined))

(define the-lexical-spec
  '(
     (whitespace (whitespace) skip)
     (comment ("#" (arbno (not #\newline))) skip)
     (number (digit (arbno digit)) number)
     (number ("-" digit (arbno digit)) number)
     (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
   )
)

(define the-grammar
  '(
     (program ((arbno var-expr ";") expr (arbno expr)) a-program)
     (expr (number) num-expr)
     (expr ("up" "(" expr ")") up-expr)
     (expr ("down" "(" expr ")") down-expr)
     (expr ("left" "(" expr ")") left-expr)
     (expr ("right" "(" expr ")") right-expr)
     (expr ("(" expr expr ")") point-expr)
     (expr ("+" expr expr) add-expr)
     (expr ("origin?" "(" expr ")") origin-expr)
     (expr ("if" "(" expr ")" "then" expr "else" expr ) if-expr)
     (expr ("move" "(" expr expr (arbno expr) ")") move-expr)
     (expr ("{" (arbno var-expr) (arbno expr) "}") block-expr)
     (expr (identifier) iden-expr)
     (expr ("fun" "(" (arbno identifier) ")" "=" expr) fun-expr)
     (expr ("call" "(" expr (arbno expr) ")") fun-call-expr)
     (var-expr ("val" identifier "=" expr) val)
     (var-expr ("final val" identifier "=" expr) final-val)
     (var-expr ("def" identifier "(" (arbno identifier) ")" "=" expr) fun-def)
   )
)

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define (show-the-datatypes)
  (sllgen:list-define-datatypes the-lexical-spec the-grammar))

(define parser
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
