#lang typed/racket

(require typed/rackunit)
;;haven't implemented conditionals, equal?, substring, strlen, error, or given

(define-type ExprC (U NumC idC StrC LamC IfC appC))
(struct StrC ([s : String ]) #:transparent)
(struct NumC ([n : Real]) #:transparent)
(struct BinOp ([op : (U '+ '* '- '/)] [frst : ExprC] [snd : ExprC]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct IfC ([test : ExprC] [thn : ExprC] [els : ExprC]) #:transparent)
(struct appC ([fun : ExprC] [arg : (Listof ExprC)]) #:transparent)
(struct Binding ([name : Symbol] [val : Value]) #:transparent)
(define-type Env [Listof Binding])

(define-type Value (U NumV BoolV PrimV StrV CloV))
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct PrimV ([val : Symbol]) #:transparent)
(struct StrV ([s : String]) #:transparent)
(struct CloV ([params : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct LamC ([arg : (Listof Symbol)] [body : ExprC]) #:transparent)
(define top-env (list
                 (Binding 'true (BoolV true))
                 (Binding 'false (BoolV false))
                 (Binding '+ (PrimV '+))
                 (Binding '- (PrimV '-))
                 (Binding '* (PrimV '*))
                 (Binding '/ (PrimV '/))
                 (Binding '<= (PrimV '<=))
                 (Binding 'equal? (PrimV 'equal?))))
(define mt-env '())
(define extend-env cons)

;;interpretation evaluation for VEBG language
(define (interp [a : ExprC] [env : Env]) : Value
  (match a
    [(NumC n) (NumV n)]
    [(StrC s) (StrV s)]
    [(idC i) (lookup i env)]
    [(IfC test thn else)
     (match (interp test env)
       [(BoolV #t) (interp thn env)]
       [(BoolV #f) (interp else env)]
       [other (error 'VEBG-interp "if test did not evaluate to a boolean: ~e" other)])]         
    [(appC fun args)
     (define f-val (interp fun env))
     (define evaluated-args (map
                             (lambda ([a : ExprC]) (interp a env))
                             args))
     (apply-val f-val evaluated-args)]
    [(LamC params body) (CloV params body env)]))
;;---interp helper functions -------------------------------

;;accepts any VEBG4 value and returns a string
(define (serialize [val : Value]) : String
  (match val
    [(NumV a) (~v a)]
    [(StrV a) (~v a)]
    [(PrimV a) "#<primop>"]
    [(BoolV a) (if a "true" "false")]
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

;;takes a function value and list of arguments
;; evaluates the function with arguments and returns Value
(define (apply-val [fun-val : Value] [args : (Listof Value)]) : Value
  (match fun-val
    [(CloV params body env) (interp body (match-args params args env))]
    [(PrimV val) (if (eq? args '()) fun-val
                     (match* (val args)
                       [('+ (list a b)) (binop val a b)]
                       [('- (list a b)) (binop val a b)]
                       [('* (list a b)) (binop val a b)]
                       [('/ (list a b)) (binop val a b)]
                       [('equal? (list a b))
                        (binop 'equal? a b)]
                       [('<= (list a b)) (binop '<= a b)]))]
    [other (error 'VEBG-interp "cannot apply non-function: ~e" other)]))

;;takes a binary operator and two Values
;;performs operator on values and returns a Value
(define (binop op [l : Value] [r : Value]) : Value
  (match* (op l r)
    [('+ (NumV x) (NumV y))
          (NumV (+ x y))]
    [('- (NumV x) (NumV y))
          (NumV (- x y))]
    [('* (NumV x) (NumV y))
          (NumV (* x y))]
    [('/ (NumV x) (NumV y))
          (if (= y 0) (error 'VEBG-binop "cannot divide by zero")
              (NumV (/ x y)))]
    [('<= (NumV x) (NumV y))
     (BoolV (<= x y))]
    [('equal? x y) (match* (x y)
                     [((NumV x) (NumV y)) (BoolV (= x y))]
                     [((StrV x) (StrV y)) (BoolV (equal? x y))]
                     [((BoolV x) (BoolV y)) (BoolV (equal? x y))])]                          
     [(_ _ _) (error 'VEBG-binop "invalid binary operation: ~e ~e ~e"
                  op l r)]))

;;----end interp helper functions -------------------------------------

(define (parse [prog : Sexp]): ExprC
  (match prog
    [(? real? n) (NumC n)]
    [(? string? s) (StrC s)]
    [(list 'if tst thn els)
     (IfC (parse tst) (parse thn) (parse els))]
    [(list 'fn params '-> (? list? body))
     (LamC (parse-params params) (parse body))]
    [(list fun args ...)
     (appC (parse fun) (map parse args))]
    [(? symbol? a) (if (or [eq? a '->]
                           [eq? a 'if]
                           [eq? a 'fn])
                       (error 'VEBG-parse "invalid id, got ~e" a)
                       (idC a))]
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
(check-equal? (top-interp '{equal? 1 2}) "false")
(check-equal? (top-interp '{+}) "#<primop>")
(check-equal? (top-interp '{<= 0 2}) "true")
(check-equal? (top-interp '{{fn (x) -> {- 2 x}} 2})
              "0")
(check-equal? (top-interp '{{fn (h) -> {h 8}} {fn (x) -> { + x 1}}})
              "9")
(check-equal? (top-interp '{fn (x) -> {* x x}}) "#<procedure>")
(check-exn #rx"VEBG-binop: cannot divide by zero"
           (lambda () (top-interp '{{fn () -> {/ 1 0}}})))
(check-exn #rx"VEBG-parse: params must be a list of symbols: '\\(1\\)"
            (lambda () (top-interp '{{fn (1) -> {/ 1 1}} 1})))
(check-exn #rx"VEBG-binop: invalid binary operation: '\\+ \\(NumV 2\\) \\(PrimV '-\\)"
           (lambda () (top-interp '{ {fn (x y) -> {+ x -}} 2 2})))
 
;;If tests
(check-equal?
 (interp (IfC (idC 'true) (NumC 1) (NumC 2)) top-env)
 (NumV 1))
(check-equal?
 (interp (IfC (idC 'false) (NumC 1) (NumC 2)) top-env)
 (NumV 2))
(check-equal?
 (interp (IfC (appC (idC '<=) (list (NumC 0) (NumC 5))) (NumC 1) (NumC 2)) top-env)
 (NumV 1))
(check-equal?
 (interp (IfC (appC (idC '<=) (list (NumC 5) (NumC 0))) (NumC 1) (NumC 2)) top-env)
 (NumV 2))
;; if via top-interp
(check-equal? (top-interp '{if true "yes" "no"}) "\"yes\"")
(check-equal? (top-interp '{if false "yes" "no"}) "\"no\"")
(check-equal? (top-interp '{if {<= 1 2} 10 20}) "10")
;; non-boolean test should error
(check-exn #rx"VEBG-interp: if test did not evaluate to a boolean"
           (lambda () (interp (IfC (NumC 5) (NumC 1) (NumC 2)) top-env)))
;; parse guards reserved words
(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse 'if)))
 
;;parse tests
(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse 'fn)))
(check-exn #rx"VEBG-parse: expected valid syntax, got #t"
           (lambda () (parse true))) 
 
;;serialize tests
(check-equal? (serialize (NumV 34)) "34")
(check-equal? (serialize (BoolV true)) "true")
(check-equal? (serialize (BoolV false)) "false")
 
;;interp tests
#;(check-equal? (interp (idC '+) top-env) "+")
(check-equal? (interp (appC (idC '+) (list (appC (LamC '(x y)
                                               (appC (idC '-) (list (idC 'x) (idC 'y))))
                                      (list (NumC 2) (NumC 5)))
                             (appC (LamC '(x)
                                            (appC (idC '/)
                                                   (list (appC (idC '*)
                                                               (list (NumC 2) (idC 'x)))
                                                   (NumC 2))))
                                   (list (NumC 10)))))
                      top-env)
              (NumV 7))
 
 
(check-equal? (interp (appC (idC '+) (list (NumC 10)
                             (appC (LamC '(x y)
                                            (appC (idC '*) (list (idC 'x) (idC 'y))))
                                   (list (NumC 1) (NumC 2)))))
                      top-env)
              (NumV 12))
(check-exn #rx"VEBG-interp: cannot apply non-function"
           (lambda () (interp (appC (NumC 3) (list (NumC 4))) top-env)))
 
(check-exn #rx"VEBG-interp: input mismatch, too many argument\\(s\\): \\(list \\(NumV 3\\)\\)"
           (lambda () (top-interp '{{fn (x) -> (* x 2)} 2 3})))
(check-exn #rx"VEBG-interp: input mismatch, missing argument\\(s\\): '\\(z\\)"
           (lambda () (top-interp '{{fn (x y z) -> (* x 2)} 2 3})))
 
(check-exn #rx"VEBG-interp: cannot apply non-function: \\(NumV 7\\)"
           (lambda () (apply-val (NumV 7) '())))
 
(check-exn #rx"VEBG-interp: cannot apply non-function"
           (lambda () (interp (appC (NumC 3) (list (NumC 4))) mt-env)))
 
(check-exn #rx"VEBG-interp-lookup: name not found: 'missing"
           (lambda () (lookup 'missing mt-env)))

;;interp - StrC
(check-equal? (interp (StrC "hello") mt-env) (StrV "hello"))
 
;;serialize - StrV
(check-equal? (serialize (StrV "hello")) "\"hello\"")
 
;;lookup - skips non-matching binding before finding target
(check-equal? (lookup 'b (list (Binding 'a (NumV 1)) (Binding 'b (NumV 2)))) (NumV 2))
 
;;match-args - both empty returns env unchanged
(check-equal? (match-args '() '() mt-env) mt-env)
;;match-args - matching params to args builds correct env
(check-equal? (match-args '(x) (list (NumV 5)) mt-env)
              (list (Binding 'x (NumV 5))))
 
;;binop - equal? on NumV, StrV, BoolV
(check-equal? (binop 'equal? (NumV 3) (NumV 3)) (BoolV #t))
(check-equal? (binop 'equal? (NumV 3) (NumV 4)) (BoolV #f))
(check-equal? (binop 'equal? (StrV "a") (StrV "a")) (BoolV #t))
(check-equal? (binop 'equal? (StrV "a") (StrV "b")) (BoolV #f))
(check-equal? (binop 'equal? (BoolV #t) (BoolV #t)) (BoolV #t))
(check-equal? (binop 'equal? (BoolV #t) (BoolV #f)) (BoolV #f))
 
;;parse - string literal
(check-equal? (parse '"hello") (StrC "hello"))
;;parse - valid symbol becomes idC
(check-equal? (parse 'x) (idC 'x))
;;parse - -> is a reserved word
(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse '->)))