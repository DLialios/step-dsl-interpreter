#lang racket
(#%require (lib "eopl.ss" "eopl"))
(#%require "lang.rkt")
(#%provide (all-defined))

(define-datatype expressed-val expressed-val?
  (num-val (n number?))
  (bool-val (b boolean?))
  (step-val (s step?))
  (point-val (p point?))
  (proc-val (p proc?))
)

(define-datatype step step?
  (up-step (num number?))
  (down-step (num number?))
  (left-step (num number?))
  (right-step (num number?))
)

(define (point? p)
  (and (list? p)
       (= 3 (length p))
       (equal? (first p) 'point)
       (number? (second p))
       (number? (third p)))
)

(define-datatype proc proc?
  (procedure (params (list-of symbol?))
             (body-expr expr?)
             (saved-env environment?))
)

;;;;extractors
(define (invalid-args-exception fun-name expected-val actual-val)
  (raise (~a fun-name ", expected: " expected-val ", got: " actual-val) ))

(define (num-val->n num)
  (or (expressed-val? num) (invalid-args-exception "num-val->n" "expressed-val?" num))
  (cases expressed-val num
    (num-val (n) n)
    (else (invalid-args-exception "num-val->n" "num-val?" num)))
)

(define (bool-val->b bool)
  (or (expressed-val? bool) (invalid-args-exception "bool-val->b" "expressed-val?" bool))
  (cases expressed-val bool
    (bool-val (b) b)
    (else (invalid-args-exception "bool-val->b" "bool-val?" bool)))
)

(define (step-val->st st)
  (or (expressed-val? st) (invalid-args-exception "step-val->st" "expressed-val?" st))
  (cases expressed-val st
    (step-val (st) st)
    (else (invalid-args-exception "step-val->st" "num-val?" st)))
)

(define (point-val->p p)
  (or (expressed-val? p) (invalid-args-exception "point-val->p" "expressed-val?" p))
  (cases expressed-val p
    (point-val (p) p)
    (else (invalid-args-exception "point-val->p" "point-val?" p)))
)

(define (proc-val->p p)
  (or (expressed-val? p) (invalid-args-exception "proc-val->p" "expressed-val?" p))
  (cases expressed-val p
    (proc-val (p) p)
    (else (invalid-args-exception "proc-val->p" "proc-val?" p)))
)

;;;;step
(define (up-step? st)
  (and (step? st)
       (cases step st
         (up-step (num) #t)
         (else #f)))
)

(define (down-step? st)
  (and (step? st)
       (cases step st
         (down-step (num) #t)
         (else #f))) 
)

(define (left-step? st)
  (and (step? st)
       (cases step st
         (left-step (num) #t)
         (else #f))) 
)

(define (right-step? st)
  (and (step? st)
       (cases step st
         (right-step (num) #t)
         (else #f)))  
)

(define (single-step->n st)
  (if (step? st)
      (cases step st
        (up-step (num) num)
        (down-step (num) num)
        (left-step (num) num)
        (right-step (num) num)
        (else (invalid-args-exception "single-step->n" "single-step?" st)))
      (raise (invalid-args-exception "single-step->n" "single-step?" st)))
)

;;;;point
(define (point x y)
  (or (number? x) (raise (invalid-args-exception "point" "number" x)))
  (or (number? y) (raise (invalid-args-exception "point" "number" y)))
  (list 'point x y)
)

(define (point->x p)
  (if (point? p)
      (second p)
      (raise (invalid-args-exception "point->x" "point?" p)))
)

(define (point->y p)
  (if (point? p)
      (third p)
      (raise (invalid-args-exception "point->x" "point?" p)))
)

;;;;environment
(define-datatype environment environment?
  (empty-env)
  (extend-env       (bvar symbol?)
                    (bval expressed-val?)
                    (saved-env environment?))
  (extend-env-final (bvar symbol?)
                    (bval expressed-val?)
                    (saved-env environment?))
)

(define (no-binding-exception sym)
  (raise (~a "No binding for '" sym))
)

(define (sym-final-exception sym)
  (raise (~a "variable '" sym " is final and cannot be overridden."))
)

(define (extend-env-wrapper sym val old-env final?)
  (define (is-var-final? env)
    (cases environment env
      (empty-env () #f)
      (extend-env (sym val prev-env) (is-var-final? prev-env))
      (extend-env-final (stored-sym val prev-env)
        (if (equal? sym stored-sym)
            #t
            (is-var-final? prev-env))))
  )
  (if (is-var-final? old-env)
      (sym-final-exception sym)
      (if final?
          (extend-env-final sym val old-env)
          (extend-env sym val old-env)))
)

(define (apply-env env search-sym)
  (cases environment env
    (empty-env () (no-binding-exception search-sym))
    (extend-env (var val saved-env)
      (if (eqv? search-sym var)
          val
          (apply-env saved-env search-sym)))
    (extend-env-final (var val saved-env)
      (if (eqv? search-sym var)
          val
          (apply-env saved-env search-sym))))
)
