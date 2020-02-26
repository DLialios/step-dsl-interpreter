#lang racket
(#%provide (all-defined))
(#%require (lib "eopl.ss" "eopl"))
(#%require "env-values.rkt")
(#%require "lang.rkt")

(define (run program-string)
  (if (string? program-string)
      (value-of (parser program-string) (empty-env))
      (raise (string-append "expected a program as string, got: " (~a program-string))))  
)

(define (value-of ast env)
  (cond [(program? ast) (value-of-program ast env)]
        [(expr? ast) (value-of-expr ast env)]
        [(var-expr? ast) (value-of-var ast env)]
        [else (raise (~a "Unimplemented ast node: " ~a ast))])
)

(define (value-of-program prog env)
  (cases program prog
    (a-program (var-exprs expr rest-of-expressions)
      (let ([updated-env (foldl value-of env var-exprs)])
        (andmap (lambda (ex) (value-of ex updated-env))
                (flatten (list expr rest-of-expressions))))))
)

(define (value-of-expr ex env)
  (or (expr? ex) (raise (string-append "value-of-expr error: expected an expression, got " (~a ex))))
  (cases expr ex
    (num-expr (n) (num-val n))
    (up-expr (num) (step-val (up-step (num-val->n (value-of num env)))))
    (down-expr (num) (step-val (down-step (num-val->n (value-of num env)))))
    (left-expr (num) (step-val (left-step (num-val->n (value-of num env)))))
    (right-expr (num) (step-val (right-step (num-val->n (value-of num env)))))
    (iden-expr (var-name) (apply-env env var-name))
    (point-expr (x y) (point-val (point (num-val->n (value-of x env)) (num-val->n (value-of y env)))))
    (move-expr (point-expr first-move rest-of-moves)
      (letrec ([start-p (point-val->p (value-of point-expr env))]
               [all-moves-as-expr (map (lambda (ex) (value-of ex env)) (flatten (list first-move rest-of-moves)))]
               [all-moves-step (map step-val->st all-moves-as-expr)]
               [final-p (foldl move start-p all-moves-step)])
        (point-val final-p)))
    (add-expr (lhs rhs)
      (letrec ([l-step-val (value-of lhs env)]
               [r-step-val (value-of rhs env)]
               [l-step (step-val->st l-step-val)]
               [r-step (step-val->st r-step-val)]
               [res (+ (get-axis-value l-step) (get-axis-value r-step))])
        (cond [(and (valid-steps-for-add? l-step r-step) 
                    (or (left-step? l-step) (right-step? l-step))) 
               (get-horizontal-step res)]
              [(and (valid-steps-for-add? l-step r-step) (or (up-step? l-step) (down-step? l-step)))
               (get-vertical-step res)]
              [else (raise "invalid args in add")])))
    (origin-expr (p-expr) (bool-val (equal? (point-val->p (value-of p-expr env)) (point 0 0))))
    (if-expr (cond then-exp else-exp)
      (let ([c-val (bool-val->b (value-of cond env))])
        (if c-val
            (value-of then-exp env)
            (value-of else-exp env))))
    (block-expr (list-of-var-decl list-of-expr)
      (let ([new-env (foldl value-of env list-of-var-decl)])
        (andmap (lambda (ex) (value-of ex new-env))
                list-of-expr)))
    (fun-expr (syms expr)
      (proc-val (procedure syms expr env)))
    (fun-call-expr (expr exprs)
      (cases proc (proc-val->p (value-of expr env))
        (procedure (params body-expr saved-env)
          (if (not (= (length params) (length exprs)))
              (raise (~a "arity mismatch. expected " (length params) " arguments, received " (length exprs)))
              (value-of body-expr
                        (foldl (lambda (sym val default) (extend-env-wrapper sym (value-of val env) default #t))
                               saved-env
                               params exprs))))))
    (else (raise (~a "value-of-expr error: unimplemented expression: " ex))))
)

(define (value-of-var v-ex env)
  (or (var-expr? v-ex) (invalid-args-exception "value-of-var" "var-expr?" v-ex))
  (cases var-expr v-ex
    (val (iden val-of-iden)
      (extend-env-wrapper iden (value-of val-of-iden env) env #f))
    (final-val (iden val-of-iden)
      (extend-env-wrapper iden (value-of val-of-iden env) env #t))
    (fun-def (sym syms expr)
      (extend-env-wrapper sym (proc-val (procedure syms expr env)) env #t))
    (else (raise (~a "value-of-var error: unimplemented expression: " v-ex))))
)

;;;;helpers
(define (move st start-p)
  (cases step st
    (up-step (st) (point (point->x start-p) (+ (point->y start-p) st)))
    (down-step (st) (point (point->x start-p) (- (point->y start-p) st)))    
    (left-step (st) (point ( - (point->x start-p) st) (point->y start-p)))    
    (right-step (st) (point ( + (point->x start-p) st) (point->y start-p))))
)

(define (valid-steps-for-add? st1 st2)
  (or (and (up-step? st1) (up-step? st2))
      (and (down-step? st1) (down-step? st2))
      (and (up-step? st1) (down-step? st2))
      (and (down-step? st1) (up-step? st2)) 
      (and (left-step? st1) (left-step? st2))
      (and (right-step? st1) (right-step? st2))
      (and (left-step? st1) (right-step? st2))
      (and (right-step? st1) (left-step? st2)))
)

(define (get-axis-value st)
  (cases step st
    (up-step (st) st)
    (down-step (st) (* -1 st))
    (left-step (st) (* -1 st))
    (right-step (st) st))
)

(define (get-vertical-step num)
  (if (positive? num)
      (step-val (up-step num)) 
      (step-val (down-step (* -1 num))))         
)

(define (get-horizontal-step num)
  (if (positive? num)
      (step-val (right-step num)) 
      (step-val (left-step (* -1 num))))         
)
