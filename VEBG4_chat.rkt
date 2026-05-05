#lang typed/racket

;; Full project implemented.
;; AI-AUDIT NOTE: The uploaded starter was a VEBG3-style file. The VEBG4 code
;; below is AI-written or AI-changed unless a nearby comment says it was kept
;; from the starter style.

(require typed/rackunit)

;; AI-ADDED-FOR-VEBG4: Export definitions so a grader/test file can require them.
(provide (all-defined-out))

;; ============================================================================
;; AI-CHANGED-FOR-VEBG4: Core syntax and values
;; ============================================================================

;; AI-CHANGED-FOR-VEBG4: ExprC represents the parsed core language expression.
(define-type ExprC (U NumC StrC idC IfC FnC appC))

(struct NumC ([n : Real]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct IfC ([test : ExprC] [thn : ExprC] [els : ExprC]) #:transparent)
(struct FnC ([params : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct appC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)

;; AI-CHANGED-FOR-VEBG4: Value represents every value the interpreter can return.
(define-type Value (U NumV BoolV StrV CloV PrimV))

(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StrV ([s : String]) #:transparent)
(struct PrimV ([name : Symbol]) #:transparent)
(struct Binding ([name : Symbol] [val : Value]) #:transparent)

;; AI-CHANGED-FOR-VEBG4: Environment is a list of name/value bindings.
(define-type Environment (Listof Binding))

;; AI-CHANGED-FOR-VEBG4: Env is kept as a short alias for the starter-file style.
(define-type Env Environment)

(struct CloV ([params : (Listof Symbol)]
              [body : ExprC]
              [env : Environment]) #:transparent)
(struct GivenBind ([name : Symbol] [rhs : ExprC]) #:transparent)

;; AI-CHANGED-FOR-VEBG4: mt-env is the empty environment.
(: mt-env Environment)
(define mt-env '())

;; AI-CHANGED-FOR-VEBG4: extend-env returns env with one new binding added first.
(: extend-env (Binding Environment -> Environment))
(define (extend-env [b : Binding] [env : Environment]) : Environment
  (cons b env))

;; ============================================================================
;; AI-ADDED-FOR-VEBG4: Small general helpers
;; ============================================================================

;; AI-ADDED-FOR-VEBG4: vebg-error raises a VEBG error with the supplied message.
(: vebg-error (Symbol String -> Nothing))
(define (vebg-error [who : Symbol] [msg : String]) : Nothing
  (error who "VEBG: ~a" msg))

;; AI-ADDED-FOR-VEBG4: reserved-id? is true for words that cannot be identifiers.
(: reserved-id? (Symbol -> Boolean))
(define (reserved-id? [s : Symbol]) : Boolean
  (or (symbol=? s 'if)
      (symbol=? s '=)
      (symbol=? s 'given)
      (symbol=? s 'fn)
      (symbol=? s '->)
      (symbol=? s 'do)))

;; AI-CHANGED-FOR-VEBG4: has-duplicates? returns true when a symbol list repeats.
(: has-duplicates? ((Listof Symbol) -> Boolean))
(define (has-duplicates? [xs : (Listof Symbol)]) : Boolean
  (cond
    [(empty? xs) #f]
    [(member (first xs) (rest xs)) #t]
    [else (has-duplicates? (rest xs))]))

;; AI-ADDED-FOR-VEBG4: check-id-list! rejects reserved words and duplicate names.
(: check-id-list! ((Listof Symbol) Sexp String -> Void))
(define (check-id-list! [ids : (Listof Symbol)]
                        [whole : Sexp]
                        [where : String]) : Void
  (cond
    [(ormap reserved-id? ids)
     (vebg-error 'VEBG-parse
                 (format "reserved word used as a ~a in ~e: ~e"
                         where whole ids))]
    [(has-duplicates? ids)
     (vebg-error 'VEBG-parse
                 (format "duplicate ~a name in ~e: ~e" where whole ids))]
    [else (void)]))

;; ============================================================================
;; AI-ADDED-FOR-VEBG4: Parser and given desugaring
;; ============================================================================

;; AI-ADDED-FOR-VEBG4: parse-param-list converts a fn parameter sexp to symbols.
(: parse-param-list (Sexp Sexp -> (Listof Symbol)))
(define (parse-param-list [raw : Sexp] [whole : Sexp]) : (Listof Symbol)
  (match raw
    [(list (? symbol? ids) ...)
     (define params (cast ids (Listof Symbol)))
     (check-id-list! params whole "function parameter")
     params]
    [other
     (vebg-error 'VEBG-parse
                 (format "fn parameters must be a list of identifiers in ~e, got ~e"
                         whole other))]))

;; AI-ADDED-FOR-VEBG4: parse-given-binding parses one [id = expr] binding.
(: parse-given-binding (Sexp Sexp -> GivenBind))
(define (parse-given-binding [b : Sexp] [whole : Sexp]) : GivenBind
  (match b
    [(list (? symbol? name) '= rhs)
     (cond
       [(reserved-id? name)
        (vebg-error 'VEBG-parse
                    (format "reserved word used as a given binding name in ~e: ~e"
                            whole name))]
       [else (GivenBind name (parse rhs))])]
    [other
     (vebg-error 'VEBG-parse
                 (format "given binding must look like [id = expr] in ~e, got ~e"
                         whole other))]))

;; AI-ADDED-FOR-VEBG4: parse-given-bindings parses the full given binding list.
(: parse-given-bindings (Sexp Sexp -> (Listof GivenBind)))
(define (parse-given-bindings [raw : Sexp]
                              [whole : Sexp]) : (Listof GivenBind)
  (match raw
    [(list bindings ...)
     (define raw-bindings (cast bindings (Listof Sexp)))
     (define parsed
       (map (lambda ([b : Sexp]) : GivenBind
              (parse-given-binding b whole))
            raw-bindings))
     (define names (map GivenBind-name parsed))
     (check-id-list! names whole "given binding")
     parsed]
    [other
     (vebg-error 'VEBG-parse
                 (format "given must contain a list of bindings in ~e, got ~e"
                         whole other))]))

;; AI-CHANGED-FOR-VEBG4: parse turns a VEBG4 s-expression into a core ExprC.
(: parse (Sexp -> ExprC))
(define (parse [prog : Sexp]) : ExprC
  (match prog
    [(? real? n) (NumC n)]
    [(? string? s) (StrC s)]
    [(? symbol? s)
     (cond
       [(reserved-id? s)
        (vebg-error 'VEBG-parse
                    (format "reserved word cannot be used as an identifier in ~e" prog))]
       [else (idC s)])]
    [(list 'if tst thn els)
     (IfC (parse tst) (parse thn) (parse els))]
    [(list 'if bad-parts ...)
     (vebg-error 'VEBG-parse
                 (format "if must have exactly a test, then, and else expression in ~e"
                         prog))]
    [(list 'fn params '-> body)
     (FnC (parse-param-list params prog) (parse body))]
    [(list 'fn bad-parts ...)
     (vebg-error 'VEBG-parse
                 (format "fn must look like {fn (id ...) -> expr}, got ~e" prog))]
    [(list 'given bindings 'do body)
     (define parsed-bindings (parse-given-bindings bindings prog))
     (define names (map GivenBind-name parsed-bindings))
     (define rhss (map GivenBind-rhs parsed-bindings))
     ;; AI-ADDED-FOR-VEBG4: given desugars to immediate function application.
     (appC (FnC names (parse body)) rhss)]
    [(list 'given bad-parts ...)
     (vebg-error 'VEBG-parse
                 (format "given must look like {given {[id = expr] ...} do expr}, got ~e"
                         prog))]
    [(list fun args ...)
     (appC (parse fun) (map parse (cast args (Listof Sexp))))]
    [other
     (vebg-error 'VEBG-parse
                 (format "expected a valid VEBG4 expression, got ~e" other))]))

;; ============================================================================
;; AI-CHANGED-FOR-VEBG4: Environments, values, and primitives
;; ============================================================================

;; AI-CHANGED-FOR-VEBG4: lookup returns the most recent value bound to query.
(: lookup (Symbol Environment -> Value))
(define (lookup [query : Symbol] [env : Environment]) : Value
  (match env
    ['()
     (vebg-error 'VEBG-lookup
                 (format "unbound identifier ~e" query))]
    [(cons (Binding name val) rst)
     (cond
       [(symbol=? query name) val]
       [else (lookup query rst)])]))

;; AI-CHANGED-FOR-VEBG4: bind-params pairs function parameters with argument values.
(: bind-params ((Listof Symbol) (Listof Value) Environment ExprC -> Environment))
(define (bind-params [params : (Listof Symbol)]
                     [args : (Listof Value)]
                     [base-env : Environment]
                     [whole-app : ExprC]) : Environment
  (match* (params args)
    [('() '()) base-env]
    [((cons p ps) (cons a as))
     (extend-env (Binding p a)
                 (bind-params ps as base-env whole-app))]
    [(_ _)
     (vebg-error 'VEBG-interp
                 (format "wrong number of arguments in application ~e; expected ~a, got ~a"
                         whole-app (length params) (length args)))]))

;; AI-ADDED-FOR-VEBG4: serialize converts any VEBG4 value to its printed string.
(: serialize (Value -> String))
(define (serialize [v : Value]) : String
  (match v
    [(NumV n) (~v n)]
    [(BoolV #t) "true"]
    [(BoolV #f) "false"]
    [(StrV s) (~v s)]
    [(CloV _ _ _) "#<procedure>"]
    [(PrimV _) "#<primop>"]))

;; AI-ADDED-FOR-VEBG4: value->number extracts a number for numeric primitives.
(: value->number (Value Symbol ExprC -> Real))
(define (value->number [v : Value]
                       [prim-name : Symbol]
                       [whole-app : ExprC]) : Real
  (match v
    [(NumV n) n]
    [other
     (vebg-error 'VEBG-interp
                 (format "primitive ~e expected a number in ~e, got ~a"
                         prim-name whole-app (serialize other)))]))

;; AI-ADDED-FOR-VEBG4: value->string extracts a string for string primitives.
(: value->string (Value Symbol ExprC -> String))
(define (value->string [v : Value]
                       [prim-name : Symbol]
                       [whole-app : ExprC]) : String
  (match v
    [(StrV s) s]
    [other
     (vebg-error 'VEBG-interp
                 (format "primitive ~e expected a string in ~e, got ~a"
                         prim-name whole-app (serialize other)))]))

;; AI-ADDED-FOR-VEBG4: value->natural extracts an exact nonnegative integer index.
(: value->natural (Value Symbol ExprC -> Natural))
(define (value->natural [v : Value]
                        [prim-name : Symbol]
                        [whole-app : ExprC]) : Natural
  (match v
    [(NumV n)
     (cond
       [(exact-nonnegative-integer? n) n]
       [else
        (vebg-error 'VEBG-interp
                    (format "primitive ~e expected a natural number in ~e, got ~a"
                            prim-name whole-app (serialize v)))])]
    [other
     (vebg-error 'VEBG-interp
                 (format "primitive ~e expected a natural number in ~e, got ~a"
                         prim-name whole-app (serialize other)))]))

;; AI-ADDED-FOR-VEBG4: prim-arity-error reports wrong primitive argument counts.
(: prim-arity-error (Symbol Natural (Listof Value) ExprC -> Nothing))
(define (prim-arity-error [name : Symbol]
                          [expected : Natural]
                          [args : (Listof Value)]
                          [whole-app : ExprC]) : Nothing
  (vebg-error 'VEBG-interp
              (format "primitive ~e got the wrong number of arguments in ~e; expected ~a, got ~a"
                      name whole-app expected (length args))))

;; AI-ADDED-FOR-VEBG4: vebg-equal? implements VEBG4 equal? for non-functions.
(: vebg-equal? (Value Value -> Boolean))
(define (vebg-equal? [a : Value] [b : Value]) : Boolean
  (match* (a b)
    [((NumV x) (NumV y)) (= x y)]
    [((BoolV x) (BoolV y)) (equal? x y)]
    [((StrV x) (StrV y)) (equal? x y)]
    [(_ _) #f]))

;; AI-ADDED-FOR-VEBG4: apply-prim evaluates a primitive operator on argument values.
(: apply-prim (Symbol (Listof Value) ExprC -> Value))
(define (apply-prim [name : Symbol]
                    [args : (Listof Value)]
                    [whole-app : ExprC]) : Value
  (match name
    ['+
     (match args
       [(list a b)
        (NumV (assert (+ (value->number a name whole-app)
                       (value->number b name whole-app))
                      real?))]
       [_ (prim-arity-error name 2 args whole-app)])]
    ['-
     (match args
       [(list a b)
        (NumV (assert (- (value->number a name whole-app)
                       (value->number b name whole-app))
                      real?))]
       [_ (prim-arity-error name 2 args whole-app)])]
    ['*
     (match args
       [(list a b)
        (NumV (assert (* (value->number a name whole-app)
                       (value->number b name whole-app))
                      real?))]
       [_ (prim-arity-error name 2 args whole-app)])]
    ['/
     (match args
       [(list a b)
        (define left (value->number a name whole-app))
        (define right (value->number b name whole-app))
        (cond
          [(zero? right)
           (vebg-error 'VEBG-interp
                       (format "division by zero in ~e" whole-app))]
          [else (NumV (assert (/ left right) real?))])]
       [_ (prim-arity-error name 2 args whole-app)])]
    ['<=
     (match args
       [(list a b)
        (BoolV (<= (value->number a name whole-app)
                   (value->number b name whole-app)))]
       [_ (prim-arity-error name 2 args whole-app)])]
    ['substring
     (match args
       [(list s start stop)
        (define str (value->string s name whole-app))
        (define start-index (value->natural start name whole-app))
        (define stop-index (value->natural stop name whole-app))
        (cond
          [(> start-index stop-index)
           (vebg-error 'VEBG-interp
                       (format "substring stop index comes before start index in ~e"
                               whole-app))]
          [(> stop-index (string-length str))
           (vebg-error 'VEBG-interp
                       (format "substring index out of range for ~e in ~e"
                               str whole-app))]
          [else (StrV (substring str start-index stop-index))])]
       [_ (prim-arity-error name 3 args whole-app)])]
    ['strlen
     (match args
       [(list s)
        (NumV (string-length (value->string s name whole-app)))]
       [_ (prim-arity-error name 1 args whole-app)])]
    ['equal?
     (match args
       [(list a b) (BoolV (vebg-equal? a b))]
       [_ (prim-arity-error name 2 args whole-app)])]
    ['error
     (match args
       [(list v)
        (vebg-error 'VEBG-user-error
                    (format "user-error: ~a" (serialize v)))]
       [_ (prim-arity-error name 1 args whole-app)])]
    [other
     (vebg-error 'VEBG-interp
                 (format "unknown primitive operator ~e in ~e" other whole-app))]))

;; AI-ADDED-FOR-VEBG4: apply-value applies either a closure or primitive value.
(: apply-value (Value (Listof Value) ExprC -> Value))
(define (apply-value [fun-val : Value]
                     [arg-vals : (Listof Value)]
                     [whole-app : ExprC]) : Value
  (match fun-val
    [(CloV params body saved-env)
     (interp body (bind-params params arg-vals saved-env whole-app))]
    [(PrimV name) (apply-prim name arg-vals whole-app)]
    [other
     (vebg-error 'VEBG-interp
                 (format "attempted to apply a non-function in ~e, got ~a"
                         whole-app (serialize other)))]))

;; AI-ADDED-FOR-VEBG4: top-env binds booleans and primitive operators as values.
(: top-env Environment)
(define top-env
  (list (Binding '+ (PrimV '+))
        (Binding '- (PrimV '-))
        (Binding '* (PrimV '*))
        (Binding '/ (PrimV '/))
        (Binding '<= (PrimV '<=))
        (Binding 'substring (PrimV 'substring))
        (Binding 'strlen (PrimV 'strlen))
        (Binding 'equal? (PrimV 'equal?))
        (Binding 'error (PrimV 'error))
        (Binding 'true (BoolV #t))
        (Binding 'false (BoolV #f))))

;; ============================================================================
;; AI-CHANGED-FOR-VEBG4: Interpreter and public interface
;; ============================================================================

(define (apply-value [fun-val : Value]
                     [arg-vals : (Listof Value)]
                     [whole-app : ExprC]) : Value
  (match fun-val
    [(CloV params body saved-env)
     (interp body (bind-params params arg-vals saved-env whole-app))]
    [(PrimV name) (apply-prim name arg-vals whole-app)]
    [other
     (vebg-error 'VEBG-interp
                 (format "attempted to apply a non-function in ~e, got ~a"
                         whole-app (serialize other)))]))
;; AI-CHANGED-FOR-VEBG4: interp evaluates an ExprC in the given environment.
(: interp (ExprC Environment -> Value))
(define (interp [e : ExprC] [env : Environment]) : Value
  (match e
    [(NumC n) (NumV n)]
    [(StrC s) (StrV s)]
    [(idC s) (lookup s env)]
    [(IfC tst thn els)
     (match (interp tst env)
       [(BoolV #t) (interp thn env)]
       [(BoolV #f) (interp els env)]
       [other
        (vebg-error 'VEBG-interp
                    (format "if test did not evaluate to a boolean in ~e, got ~a"
                            e (serialize other)))])]
    [(FnC params body) (CloV params body env)]
    [(appC fun args)
     (define fun-val (interp fun env))
     (define arg-vals
       (map (lambda ([arg : ExprC]) : Value
              (interp arg env))
            args))
     (apply-value fun-val arg-vals e)]))

;; AI-CHANGED-FOR-VEBG4: top-interp parses, evaluates in top-env, and serializes.
(: top-interp (Sexp -> String))
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

;; ============================================================================
;; AI-ADDED-FOR-VEBG4: Tests
;; ============================================================================

;; Tests for simple helpers.
(check-equal? (reserved-id? 'if) #t)
(check-equal? (reserved-id? '+) #f)
(check-equal? (has-duplicates? '(x y x)) #t)
(check-equal? (has-duplicates? '(x y z)) #f)
(check-equal? (parse-param-list '(x y) '{fn (x y) -> x}) '(x y))
(check-exn #rx"VEBG.*duplicate function parameter"
           (lambda () (parse-param-list '(x x) '{fn (x x) -> x})))
(check-equal? (parse-given-binding '[x = 10]
                                    '{given {[x = 10]} do x})
              (GivenBind 'x (NumC 10)))
(check-exn #rx"VEBG.*given binding"
           (lambda () (parse-given-binding '[if = 10]
                                            '{given {[if = 10]} do if})))
(check-exn #rx"VEBG.*given binding"
           (lambda () (parse-given-bindings '([x = 1] [x = 2])
                                             '{given {[x = 1] [x = 2]} do x})))

;; Tests for parsing numbers, strings, ids, if, fn, application, and given.
(check-equal? (parse 12) (NumC 12))
(check-equal? (parse "hello") (StrC "hello"))
(check-equal? (parse '+) (idC '+))
(check-exn #rx"VEBG.*reserved word"
           (lambda () (parse 'if)))
(check-equal? (parse '{if true 1 2})
              (IfC (idC 'true) (NumC 1) (NumC 2)))
(check-exn #rx"VEBG.*if"
           (lambda () (parse '{if true 1})))
(check-equal? (parse '{fn (x y) -> {+ x y}})
              (FnC '(x y)
                   (appC (idC '+) (list (idC 'x) (idC 'y)))))
(check-exn #rx"VEBG.*duplicate function parameter"
           (lambda () (parse '{fn (x x) -> x})))
(check-equal? (parse '{+ 1 2})
              (appC (idC '+) (list (NumC 1) (NumC 2))))
(check-equal? (parse '{given {[z = {+ 9 14}]
                              [y = 98]}
                        do
                        {+ z y}})
              (appC (FnC '(z y)
                         (appC (idC '+) (list (idC 'z) (idC 'y))))
                    (list (appC (idC '+) (list (NumC 9) (NumC 14)))
                          (NumC 98))))
(check-exn #rx"VEBG.*fn"
           (lambda () (parse '{fn x -> x})))
(check-exn #rx"VEBG.*given"
           (lambda () (parse '{given [x = 1] do x})))
(check-exn #rx"VEBG.*expected a valid VEBG4 expression"
           (lambda () (parse '())))

;; Tests for environment lookup and binding.
(check-equal? (lookup 'x (list (Binding 'x (NumV 5)))) (NumV 5))
(check-equal? (lookup 'x (list (Binding 'x (NumV 1))
                               (Binding 'x (NumV 2))))
              (NumV 1))
(check-exn #rx"VEBG.*unbound identifier"
           (lambda () (lookup 'missing mt-env)))
(check-equal? (bind-params '(x y)
                           (list (NumV 1) (NumV 2))
                           mt-env
                           (appC (idC 'f) (list (NumC 1) (NumC 2))))
              (list (Binding 'x (NumV 1))
                    (Binding 'y (NumV 2))))
(check-exn #rx"VEBG.*wrong number of arguments"
           (lambda () (bind-params '(x y)
                                    (list (NumV 1))
                                    mt-env
                                    (appC (idC 'f) (list (NumC 1))))))

;; Tests for serialization and value extractors.
(check-equal? (serialize (NumV 34)) "34")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (StrV "abc")) "\"abc\"")
(check-equal? (serialize (CloV '(x) (idC 'x) mt-env)) "#<procedure>")
(check-equal? (serialize (PrimV '+)) "#<primop>")
(check-equal? (value->number (NumV 10) '+ (idC '+)) 10)
(check-exn #rx"VEBG.*expected a number"
           (lambda () (value->number (StrV "no") '+ (idC '+))))
(check-equal? (value->string (StrV "ok") 'strlen (idC 'strlen)) "ok")
(check-exn #rx"VEBG.*expected a string"
           (lambda () (value->string (NumV 1) 'strlen (idC 'strlen))))
(check-equal? (value->natural (NumV 3) 'substring (idC 'substring)) 3)
(check-exn #rx"VEBG.*expected a natural number"
           (lambda () (value->natural (NumV 1.5) 'substring (idC 'substring))))
(check-exn #rx"VEBG.*wrong number of arguments"
           (lambda () (prim-arity-error '+ 2 (list (NumV 1)) (idC '+))))
(check-equal? (vebg-equal? (NumV 1) (NumV 1)) #t)
(check-equal? (vebg-equal? (StrV "a") (StrV "a")) #t)
(check-equal? (vebg-equal? (PrimV '+) (PrimV '+)) #f)

;; Tests for primitive application.
(check-equal? (apply-prim '+ (list (NumV 2) (NumV 3)) (idC '+)) (NumV 5))
(check-equal? (apply-prim '- (list (NumV 8) (NumV 3)) (idC '-)) (NumV 5))
(check-equal? (apply-prim '* (list (NumV 4) (NumV 5)) (idC '*)) (NumV 20))
(check-equal? (apply-prim '/ (list (NumV 8) (NumV 2)) (idC '/)) (NumV 4))
(check-exn #rx"VEBG.*division by zero"
           (lambda () (apply-prim '/ (list (NumV 1) (NumV 0)) (idC '/))))
(check-equal? (apply-prim '<= (list (NumV 2) (NumV 3)) (idC '<=)) (BoolV #t))
(check-equal? (apply-prim 'substring
                          (list (StrV "abcdef") (NumV 1) (NumV 4))
                          (idC 'substring))
              (StrV "bcd"))
(check-equal? (apply-prim 'strlen (list (StrV "abcd")) (idC 'strlen))
              (NumV 4))
(check-equal? (apply-prim 'equal? (list (StrV "a") (StrV "a")) (idC 'equal?))
              (BoolV #t))
(check-exn #rx"VEBG.*user-error.*5"
           (lambda () (apply-prim 'error (list (NumV 5)) (idC 'error))))

;; Tests for interp and top-interp.
(check-equal? (interp (NumC 10) top-env) (NumV 10))
(check-equal? (interp (StrC "x") top-env) (StrV "x"))
(check-equal? (interp (idC 'true) top-env) (BoolV #t))
(check-equal? (interp (IfC (idC 'true) (NumC 1) (NumC 2)) top-env) (NumV 1))
(check-exn #rx"VEBG.*if test"
           (lambda () (interp (IfC (NumC 0) (NumC 1) (NumC 2)) top-env)))
(check-equal? (serialize (interp (FnC '(x) (idC 'x) top-env) top-env))
              "#<procedure>")
(check-equal? (apply-value (interp (FnC '(x) (idC 'x) top-env) top-env)
                           (list (NumV 42))
                           (appC (FnC '(x) (idC 'x)) (list (NumC 42))))
              (NumV 42))
(check-exn #rx"VEBG.*non-function"
           (lambda () (apply-value (NumV 5) '() (appC (NumC 5) '()))))

(check-equal? (top-interp 5) "5")
(check-equal? (top-interp "hi") "\"hi\"")
(check-equal? (top-interp 'true) "true")
(check-equal? (top-interp 'false) "false")
(check-equal? (top-interp '+) "#<primop>")
(check-equal? (top-interp '{+ 1 2}) "3")
(check-equal? (top-interp '{- 10 4}) "6")
(check-equal? (top-interp '{* 3 7}) "21")
(check-equal? (top-interp '{/ 9 3}) "3")
(check-equal? (top-interp '{<= 3 3}) "true")
(check-equal? (top-interp '{if {<= 1 2} 10 20}) "10")
(check-equal? (top-interp '{if false 10 20}) "20")
(check-equal? (top-interp '{substring "hello" 1 4}) "\"ell\"")
(check-equal? (top-interp '{substring "hello" 2 2}) "\"\"")
(check-equal? (top-interp '{strlen "hello"}) "5")
(check-equal? (top-interp '{equal? 4 4}) "true")
(check-equal? (top-interp '{equal? "a" "a"}) "true")
(check-equal? (top-interp '{equal? true false}) "false")
(check-equal? (top-interp '{equal? + +}) "false")
(check-equal? (top-interp '{equal? {fn (x) -> x} {fn (x) -> x}}) "false")
(check-equal? (top-interp '{{fn (x) -> {+ x 1}} 9}) "10")
(check-equal? (top-interp '{{fn () -> 99}}) "99")
(check-equal? (top-interp '{{fn (f) -> {f 3 4}} +}) "7")
(check-equal? (top-interp '{{{fn (x) -> {fn (y) -> {+ x y}}} 5} 6}) "11")
(check-equal? (top-interp '{given {[z = {+ 9 14}]
                                    [y = 98]}
                              do
                              {+ z y}})
              "121")
(check-equal? (top-interp '{given {[+ = {fn (x) -> x}]}
                              do
                              {+ 5}})
              "5")
(check-equal? (top-interp '{given {[x = 10]}
                              do
                              {given {[x = 1]
                                      [y = x]}
                                do
                                y}})
              "10")
(check-equal? (top-interp '{given {[x = 10]}
                              do
                              {given {[f = {fn (y) -> {+ x y}}]}
                                do
                                {given {[x = 100]}
                                  do
                                  {f 1}}}})
              "11")

;; Error-path tests through top-interp.
(check-exn #rx"VEBG.*wrong number of arguments"
           (lambda () (top-interp '{{fn (x y) -> x} 1})))
(check-exn #rx"VEBG.*wrong number of arguments"
           (lambda () (top-interp '{+ 1})))
(check-exn #rx"VEBG.*expected a number"
           (lambda () (top-interp '{+ 1 "two"})))
(check-exn #rx"VEBG.*expected a string"
           (lambda () (top-interp '{strlen 9})))
(check-exn #rx"VEBG.*expected a natural number"
           (lambda () (top-interp '{substring "abc" 0.5 2})))
(check-exn #rx"VEBG.*out of range"
           (lambda () (top-interp '{substring "abc" 0 4})))
(check-exn #rx"VEBG.*stop index comes before start index"
           (lambda () (top-interp '{substring "abc" 2 1})))
(check-exn #rx"VEBG.*division by zero"
           (lambda () (top-interp '{/ 1 0})))
(check-exn #rx"VEBG.*if test"
           (lambda () (top-interp '{if 0 1 2})))
(check-exn #rx"VEBG.*non-function"
           (lambda () (top-interp '{5 1 2})))
(check-exn #rx"VEBG.*unbound identifier"
           (lambda () (top-interp '{given {[x = 1] [y = x]} do y})))
(check-exn #rx"VEBG.*user-error.*\"bad\""
           (lambda () (top-interp '{error "bad"})))
