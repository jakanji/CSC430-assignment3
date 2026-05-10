#lang typed/racket

(require typed/rackunit)
;;haven't implemented given

(define-type ExprC (U NumC idC StrC LamC IfC appC))
(struct StrC ([s : String ]) #:transparent)
(struct NumC ([n : Real]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct IfC ([test : ExprC] [thn : ExprC] [els : ExprC]) #:transparent)
(struct appC ([fun : ExprC] [arg : (Listof ExprC)]) #:transparent)
(struct Binding ([name : Symbol] [val : Value]) #:transparent)
(define-type Env [Listof Binding])

(define-type Value (U NumV BoolV PrimV StrV CloV))
(struct LamC ([arg : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct PrimV ([val : Symbol]) #:transparent)
(struct StrV ([s : String]) #:transparent)
(struct CloV ([params : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct GivenBind ([name : Symbol] [rhs : ExprC]) #:transparent)
(define top-env (list
                 (Binding 'true (BoolV true))
                 (Binding 'false (BoolV false))
                 (Binding '+ (PrimV '+))
                 (Binding '- (PrimV '-))
                 (Binding '* (PrimV '*))
                 (Binding '/ (PrimV '/))
                 (Binding '<= (PrimV '<=))
                 (Binding 'equal? (PrimV 'equal?))
                 (Binding 'substring (PrimV 'substring))
                 (Binding 'strlen (PrimV 'strlen))
                 (Binding 'error (PrimV 'error))
                 (Binding 'println (PrimV 'println))
                 (Binding 'read-num (PrimV 'read-num))))
(define mt-env '())
(define extend-env cons)

;;takes an s-expression and calles parser and interp
(: top-interp (Sexp -> String))
(define (top-interp fun-sexps)
  (serialize (interp (parse fun-sexps) top-env)))
 
;;accepts any VEBG4 value and returns a string
(define (serialize [val : Value]) : String
  (match val
    [(NumV a) (~v a)]
    [(StrV a) (~v a)]
    [(PrimV a) "#<primop>"]
    [(BoolV a) (if a "true" "false")]
    [(CloV _ _ _) "#<procedure>"]))

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
    [(LamC params body) (CloV params body env)]
    [(appC fun args)
     (define f-val (interp fun env))
     (define evaluated-args (map
                             (lambda ([a : ExprC]) (interp a env))
                             args))
     (apply-val f-val evaluated-args)]))

(define (parse [prog : Sexp]): ExprC
  (match prog
    [(? real? n) (NumC n)]
    [(? string? s) (StrC s)]
    [(list 'if tst thn els)
     (IfC (parse tst) (parse thn) (parse els))]
    [(list 'fn (list params ...) '-> body)
     (if (check-duplicates params)
         (error 'VEBG-parse "function cannot have duplicate parameters: ~e" params)
     (LamC (parse-params params) (parse body)))]
    [(list 'given bindings 'do body)
     (define parsed-bindings (parse-given-bindings bindings prog))
     (appC (LamC (map GivenBind-name parsed-bindings) (parse body))
           (map GivenBind-rhs parsed-bindings))]
    [(list 'given bad-parts ...)
     (error 'VEBG-parse "given must look like {given {[id = expr] ...} do expr}, got: ~e" prog)]
    [(list fun args ...)
     (appC (parse fun) (map parse args))]
    [(? symbol? a) (if (or [eq? a '->]
                           [eq? a 'if]
                           [eq? a 'fn]
                           [eq? a 'given]
                           [eq? a '=]
                           [eq? a 'do])
                       (error 'VEBG-parse "invalid id, got ~e" a)
                       (idC a))][other (error 'VEBG-parse "expected valid syntax, got ~e" other)]))

;;---interp helper functions -------------------------------

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
    [(PrimV val) (if (null? args) fun-val
                     (binop val args))]
    [other (error 'VEBG-interp "cannot apply non-function: ~e" other)]))

;;takes a binary operator and two Values
;;performs operator on values and returns a Value
(define (binop op [args : (Listof Value)]) : Value
  (match* (op args)
    [('+ (list (NumV x) (NumV y)))
          (NumV (+ x y))]
    [('- (list (NumV x) (NumV y)))
          (NumV (- x y))]
    [('* (list (NumV x) (NumV y)))
          (NumV (* x y))]
    [('/ (list (NumV x) (NumV y)))
          (if (= y 0) (error 'VEBG-binop "cannot divide by zero")
              (NumV (/ x y)))]
    [('<= (list (NumV x) (NumV y)))
     (BoolV (<= x y))]
    [('equal? (list x y)) (match* (x y)
                     [((NumV x) (NumV y)) (BoolV (= x y))]
                     [((StrV x) (StrV y)) (BoolV (equal? x y))]
                     [((BoolV x) (BoolV y)) (BoolV (equal? x y))]
                     [(_ _) (BoolV #f)])]
    [('substring (list str (NumV start) (NumV stop)))
     (match str
       [(StrV s) (apply-substring s start stop)]
       [_ (error 'VEBG-substring "first argument must be a string ~e" str)])]
    [('strlen (list str))
     (match str
       [(StrV s) (NumV (string-length s))]
       [_ (error 'VEBG-strlen "input must be a string: ~e" str)])]
    [('error (list v))
     (error 'VEBG-error "user-error: ~e" (serialize v))]
    [('println (list s))
     (println (match s
                [(NumV a) a]
                [(BoolV a) a]
                [(StrV a) a]
                [(PrimV a) a]
                [other (error 'VEBG-println
                              "cannot print value: ~e")]))
     (BoolV true)]
    [('read-num '())
     (print '>)
     (if (string->number (cast (read-line) String))
     (BoolV true) (BoolV false))]
    [(_ _) (error 'VEBG-binop "invalid binary operation: ~e ~e"
                  op args)]))

;;takes a string, a starting number, and ending number and returns a substring
;;of the string from the start to stop
(define (apply-substring [s : String] [srt : Real] [stp : Real])
  : StrV
     (cond
       [(not (exact-nonnegative-integer? srt))
        (error 'VEBG-substring "start must be exact non-negative integer: ~e" srt)]
       [(not (exact-nonnegative-integer? stp))
        (error 'VEBG-substring "start must be exact non-negative integer: ~e" stp)]
       [(> stp (string-length s))
        (error 'VEBG-substring "stop must be less than string length: ~e ~e" stp s)]
       [(< stp srt)
        (error 'VEBG-substring "stop must come after start: ~e ~e" stp srt)]
       [else (StrV (substring s srt stp))]))

;;-----helper functions for parse----------------------------------

;;takes an Sexp and returns a list of symbols
(define (parse-params [params : Sexp]) : (Listof Symbol)
  (match params
    ['() '()]
    [(cons (? symbol? f) r) (cons f (parse-params r))]
    [other (error 'VEBG-parse "params must be a list of symbols: ~e" other)]))

;;-----helper functions for given/parse-------------------------------

;;parses a single [id = expr] binding
(define (parse-given-binding [b : Sexp] [whole : Sexp]) : GivenBind
  (match b
    [(list (? symbol? name) '= rhs)
     (cond
       [(or (eq? name '->) (eq? name 'if) (eq? name 'fn)
            (eq? name 'given) (eq? name '=) (eq? name 'do))
        (error 'VEBG-parse "reserved word used as given binding name: ~e" name)]
       [else (GivenBind name (parse rhs))])]
    [other (error 'VEBG-parse "given binding must look like [id = expr], got: ~e" other)]))

;;parses a list of given bindings, checks for duplicates
(define (parse-given-bindings [raw : Sexp] [whole : Sexp]) : (Listof GivenBind)
  (match raw
    [(list bindings ...)
     (define parsed
       (map (lambda ([b : Sexp]) : GivenBind
              (parse-given-binding b whole))
            (cast bindings (Listof Sexp))))
     (if (check-duplicates (map GivenBind-name parsed))
         (error 'VEBG-parse "duplicate given binding name: ~e" (map GivenBind-name parsed))
         parsed)]
    [other (error 'VEBG-parse "given must contain a list of bindings, got: ~e" other)]))

;;----end helper functions for parse-------------------------------

;;---------------------tests----------------------------------------------------------------------------

;;top-interp tests
(check-exn #rx"function cannot have duplicate parameters: '\\(x x\\)"
 (lambda () (top-interp '{fn (x x) -> 3})))
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
(check-exn #rx"VEBG-binop: invalid binary operation: '\\+ \\(list \\(NumV 2\\) \\(PrimV '-\\)\\)"
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
(check-equal? (binop 'equal? (list (NumV 3) (NumV 3))) (BoolV #t))
(check-equal? (binop 'equal? (list (NumV 3) (NumV 4))) (BoolV #f))
(check-equal? (binop 'equal? (list (StrV "a") (StrV "a"))) (BoolV #t))
(check-equal? (binop 'equal? (list (StrV "a") (StrV "b"))) (BoolV #f))
(check-equal? (binop 'equal? (list (BoolV #t) (BoolV #t))) (BoolV #t))
(check-equal? (binop 'equal? (list (BoolV #t) (BoolV #f))) (BoolV #f))
 
;;parse - string literal
(check-equal? (parse '"hello") (StrC "hello"))
;;parse - valid symbol becomes idC
(check-equal? (parse 'x) (idC 'x))
;;parse - -> is a reserved word
(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse '->)))
 
;;substring tests
(check-equal? (top-interp '{substring "hello" 0 5}) "\"hello\"")
(check-equal? (top-interp '{substring "hello" 1 3}) "\"el\"")
(check-equal? (top-interp '{substring "hello" 0 0}) "\"\"")
;; direct binop calls
(check-equal? (binop 'substring (list (StrV "racecar") (NumV 0) (NumV 7))) (StrV "racecar"))
(check-equal? (binop 'substring (list (StrV "racecar") (NumV 3) (NumV 6))) (StrV "eca"))
;; stop > string length
(check-exn #rx"VEBG-substring: stop must be less than string length"
           (lambda () (binop 'substring (list (StrV "hello") (NumV 0) (NumV 6)))))
;; non-integer start
(check-exn #rx"VEBG-substring: start must be exact non-negative integer: 1.5"
           (lambda () (binop 'substring (list (StrV "hello") (NumV 1.5) (NumV 3)))))
;; non-integer stop (same message as start)
(check-exn #rx"VEBG-substring: start must be exact non-negative integer: 3.5"
           (lambda () (binop 'substring (list (StrV "hello") (NumV 1) (NumV 3.5)))))
;; stop before start
(check-exn #rx"VEBG-substring: stop must come after start"
           (lambda () (binop 'substring (list (StrV "hello") (NumV 3) (NumV 1)))))
;; non-string first argument
(check-exn #rx"VEBG-substring: first argument must be a string"
           (lambda () (binop 'substring (list (NumV 5) (NumV 0) (NumV 2)))))
 
 
;;strlen tests
(check-equal? (top-interp '{strlen "hello"}) "5")
(check-equal? (top-interp '{strlen ""}) "0")
(check-equal? (binop 'strlen (list (StrV "racecar"))) (NumV 7))
(check-exn #rx"VEBG-strlen: input must be a string: \\(NumV 3\\)"
           (lambda () (binop 'strlen (list (NumV 3)))))
 
 
;;error tests
(check-exn #rx"VEBG-error: user-error: \"5\""
           (lambda () (top-interp '{error 5})))
(check-exn #rx"VEBG-error: user-error: \"true\""
           (lambda () (top-interp '{error true})))
(check-exn #rx"VEBG-error: user-error: \"#<primop>\""
           (lambda () (top-interp '{error +})))
(check-exn #rx"VEBG-error: user-error: \"#<procedure>\""
           (lambda () (top-interp '{error {fn (x) -> x}})))
 
;;given tests
;; basic given with one binding
(check-equal? (top-interp '{given {[x = 5]} do x}) "5")
;; given with multiple bindings
(check-equal? (top-interp '{given {[z = {+ 9 14}] [y = 98]} do {+ z y}}) "121")
;; given desugars correctly - rhs is evaluated in outer env not inner
(check-equal? (top-interp '{given {[x = 10]}
                              do
                              {given {[x = 1] [y = x]}
                                do y}})
              "10")
;; given with shadowing - closure captures outer x=10 even when x is later rebound
(check-equal? (top-interp '{given {[x = 10]}
                              do
                              {given {[f = {fn (y) -> {+ x y}}]}
                                do
                                {given {[x = 100]}
                                  do
                                  {f 1}}}})
              "11")
;; given allows rebinding a primitive
(check-equal? (top-interp '{given {[+ = {fn (x) -> x}]} do {+ 5}}) "5")
;; parse-given-binding - valid binding
(check-equal? (parse-given-binding '[x = 10] '{given {[x = 10]} do x})
              (GivenBind 'x (NumC 10)))
;; parse-given-binding - reserved word as name
(check-exn #rx"VEBG-parse: reserved word used as given binding name"
           (lambda () (parse-given-binding '[if = 10] '{given {[if = 10]} do if})))
;; parse-given-binding - malformed binding
(check-exn #rx"VEBG-parse: given binding must look like \\[id = expr\\]"
           (lambda () (parse-given-binding '[x 10] '{given {[x 10]} do x})))
;; parse-given-bindings - duplicate names
(check-exn #rx"VEBG-parse: duplicate given binding name"
           (lambda () (parse-given-bindings '([x = 1] [x = 2]) '{given {[x = 1] [x = 2]} do x})))
;; parse - given reserved words
(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse 'given)))
(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse 'do)))
(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse '=)))
;; parse - malformed given
(check-exn #rx"VEBG-parse: given must look like"
           (lambda () (parse '{given {[x = 1]}  x}))) 
;; given - unbound in rhs (rhs evaluated in outer env, x not yet bound)
(check-exn #rx"VEBG-interp-lookup: name not found"
           (lambda () (top-interp '{given {[x = 1] [y = x]} do y})))
;; parse-given-bindings line 228 - non-list bindings sexp
(check-exn #rx"VEBG-parse: given must contain a list of bindings, got:"
           (lambda () (parse-given-bindings 42 '{given 42 do x})))

(check-equal? (top-interp '{equal? 1 "1"}) "false")