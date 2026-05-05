#lang typed/racket

(require typed/rackunit)
;;haven't implemented interp-fns yet
;;still using top-interp from lab 3

(define-type ExprC (U NumC BinOp idC FundefC appC Ifleq0C))
(struct NumC ([n : Real]) #:transparent)
(struct BinOp ([op : (U '+ '* '- '/)] [frst : ExprC] [snd : ExprC]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct appC ([fun : ExprC] [arg : (Listof ExprC)]) #:transparent)
(struct Ifleq0C ([test : ExprC] [thn : ExprC] [els : ExprC]) #:transparent)
(struct Binding ([name : Symbol] [val : Value]))
(define-type Env [Listof Binding])
(define mt-env '())
(define extend-env cons)
(define top-env (list
                 (Binding 'true true)
                 (Binding 'false false)))
(define-type Value (U Real Boolean StrV CloV))
(struct StrV ([s : String]) #:transparent)
(struct CloV ([params : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct FundefC ([arg : (Listof Symbol)] [body : ExprC]) #:transparent)

;;interpretation evaluation for VEBG language
(define (interp [a : ExprC] [env : Env]) : Value
  (match a
    [(NumC n) n]
    [(idC i) (lookup i env)]
    [(appC fun args)
     (define f-val (interp fun env))
     (define evaluated-args (map
                             (lambda ([a : ExprC]) (interp a env))
                             args))
     (apply-val f-val evaluated-args)]
    [(FundefC params body) (CloV params body env)]
    [(BinOp o l r)
     (define l-val (interp l env))
     (define r-val (interp r env))
     (cond
       [(equal? o '/)
        (if (zero? (cast r-val Real))
            (error 'VEBG-BinopTableDiv "cannot divide by zero")
            (/ (cast l-val Real) (cast r-val Real)))]
       [else
        ((BinopTable o) (cast l-val Real) (cast r-val Real))])] 
    [(Ifleq0C tst thn els)
     (if (<= (cast (interp tst
                           mt-env) Real) 0)
         (interp thn 
                 mt-env)
         (interp els 
                 mt-env))]))
;;---interp helper functions -------------------------------

;;binop lookup
(define (BinopTable [op : Symbol])
  (match op
    ['+ +]
    ['- -]
    ['* *]))

;;accepts any VEBG4 value and returns a string
(define (serialize [val : Value]) : String
  (match val
    [(? real? a) (~v a)]
    [(? boolean? a) (if a "true" "false")]
    [(CloV _ _ _) "#<procedure>"]))

;;takes a symbol to lookup and an environment
;; returns a number to bind to the symbol
(define (lookup [query : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'VEBG-interp-lookup "name not found: ~e" query)]
    [(cons (Binding name val) rst)
     (cond
       [(symbol=? query name) val]
       [else (lookup query rst)])]))

;;takes a Value, list of Values, and Env
;; returns a Value
(define (apply-val [fun-val : Value] [args : (Listof Value)]) : Value
  (match fun-val
    [(CloV params body env) (interp body (match-args params args env))]
    [other (error 'VEBG-interp "cannot apply non-function: ~e" other)]))

;;takes a list of ExprCs (arguments) and symbols (parameters)
;;returns a list of bindings of arguments to parameters
(define (match-args [params : (Listof Symbol)] [args : (Listof Value)] [env : Env])
  : Env
  (match* (params args)
    [('() '()) env]
    [((cons f1 r1) (cons f2 r2)) (extend-env (Binding f1 f2)
                                             (match-args r1 r2 env))]
    [((cons f1 r1) '()) (error 'VEBG-interp "input mismatch, missing argument(s): ~e" params )]
    [('() (cons f2 r2)) (error 'VEBG-interp "input mismatch, too many argument(s): ~e"  args)]))

;;----end interp helper functions -------------------------------------

(define (parse [prog : Sexp]): ExprC
  (match prog
    [(? real? n) (NumC n)]
    [(list '+ left right) (BinOp '+ (parse left) (parse right))]
    [(list '* left right) (BinOp '* (parse left) (parse right))]
    [(list '/ left right) (BinOp '/ (parse left) (parse right))]
    [(list '- left right) (BinOp '- (parse left) (parse right))]
    [(list 'ifleq0? tst thn els)
     (Ifleq0C (parse tst) (parse thn) (parse els))]
    [(list 'fn params '-> body)
     (FundefC (parse-params params) (parse body))]
    [(list fun args ...)
     (appC (parse fun) (map parse args))]
    [(? symbol? a) (if (or [eq? a '+]
                           [eq? a '-]
                           [eq? a '*]
                           [eq? a '/]
                           [eq? a '->]
                           [eq? a 'ifleq0?]
                           [eq? a 'named-fn])
                       (error 'VEBG-parse "invalid id, got ~e" a) (idC a))]
    [other (error 'VEBG-parse "expected valid syntax, got ~e" other)]))

;;-----helper functions for parse----------------------------------

;;takes an Sexp and returns a list of symbols
(define (parse-params [params : Sexp]) : (Listof Symbol)
  (match params
    ['() '()]
    [(cons (? symbol? f) r) (cons f (parse-params r))]
    [other (error 'VEBG-parse "params must be a list of symbols: ~e" other)]))
;;----end helper functions for parse-------------------------------

;;takes an s-expression and calles parser and interp
(: top-interp (Sexp -> String))
(define (top-interp fun-sexps)
  (serialize (interp (parse fun-sexps) top-env)))

;;---------------------tests----------------------------------------------------------------------------

;;top-interp tests
(check-equal? (top-interp '{{fn (x) -> { - 2 x}} 2})
              "0")
(check-equal? (top-interp '{{fn (h) -> {h 8}} {fn (x) -> { + x 1}}})
              "9")
(check-equal? (top-interp '{{fn () -> {ifleq0? 2 1 0}}}) "0")
(check-equal? (top-interp '{fn (x) -> {* x x}}) "#<procedure>")
(check-exn #rx"VEBG-BinopTableDiv: cannot divide by zero"
           (lambda () (top-interp '{{fn () -> {/ 1 0}}})))
(check-exn #rx"VEBG-parse: params must be a list of symbols: '\\(1\\)"
            (lambda () (top-interp '{{fn (1) -> {/ 1 1}} 1})))

;;ifleq0 tests
(check-equal?
 (interp (Ifleq0C (NumC 0) (NumC 1) (NumC 2)) mt-env)
 1)
(check-equal?
 (interp (Ifleq0C (NumC -4) (NumC 1) (NumC 2))
         mt-env)
 1)
(check-equal?
 (interp (Ifleq0C (NumC 5) (NumC 1) (NumC 2))
         mt-env)
 2)

;;parse tests
(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse '+)))
(check-exn #rx"VEBG-parse: expected valid syntax, got #t"
           (lambda () (parse true))) 

;;serialize tests
(check-equal? (serialize 34) "34")
(check-equal? (serialize true) "true")
(check-equal? (serialize false) "false")

;;interp tests
(check-equal? (interp (BinOp '+ (appC (FundefC '(x y)
                                               (BinOp '- (idC 'x) (idC 'y)))
                                      (list (NumC 2) (NumC 5)))
                             (appC (FundefC '(x)
                                            (BinOp '/ (BinOp '* (NumC 2) (idC 'x))
                                                   (NumC 2)))
                                   (list (NumC 10))))
                      mt-env)
              7)


(check-equal? (interp (BinOp '+ (NumC 10)
                             (appC (FundefC '(x y)
                                            (BinOp '* (idC 'x) (idC 'y)))
                                   (list (NumC 1) (NumC 2))))
                      mt-env)
              12)
(check-exn #rx"VEBG-interp: cannot apply non-function"
           (lambda () (interp (appC (NumC 3) (list (NumC 4))) mt-env)))

(check-exn #rx"VEBG-interp: input mismatch, too many argument\\(s\\): '\\(3\\)"
           (lambda () (top-interp '{{fn (x) -> (* x 2)} 2 3})))
(check-exn #rx"VEBG-interp: input mismatch, missing argument\\(s\\): '\\(z\\)"
           (lambda () (top-interp '{{fn (x y z) -> (* x 2)} 2 3})))

(check-exn #rx"VEBG-interp: cannot apply non-function: 7"
           (lambda () (apply-val 7 '())))

(check-exn #rx"VEBG-interp: cannot apply non-function"
           (lambda () (interp (appC (NumC 3) (list (NumC 4))) mt-env)))

(check-exn #rx"VEBG-interp-lookup: name not found: 'missing"
           (lambda () (lookup 'missing mt-env)))
