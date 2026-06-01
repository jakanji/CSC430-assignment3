#lang typed/racket

(require typed/rackunit)

;; Fully implemented assignment7

(define-type ExprC (U NumC idC StrC LamC IfC appC RebC ChainC RecC))
(struct StrC ([s : String ]) #:transparent)
(struct NumC ([n : Real]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct IfC ([test : ExprC] [thn : ExprC] [els : ExprC]) #:transparent)
(struct appC ([fun : ExprC] [arg : (Listof ExprC)]) #:transparent)
(struct RebC ([id : ExprC] [arg : ExprC]) #:transparent)
(struct ParamC ([ty : Type] [name : Symbol]) #:transparent)             ;; added for types association
(struct LamC ([arg : (Listof ParamC)] [body : ExprC]) #:transparent)     ;; changed to paramC from symbol
(struct ChainC ([exprs : (Listof ExprC)]) #:transparent)
(struct RecC ([ty : Type] [name : Symbol] [rhs : ExprC] [body : ExprC]) #:transparent)

(define-type Type (U NumT BoolT StrT funT))
(struct NumT () #:transparent)
(struct BoolT () #:transparent)
(struct StrT () #:transparent)
(struct funT ([argT : (Listof Type)] [retT : Type]) #:transparent)

(struct TBinding ([id : Symbol] [ty : Type]) #:transparent)
(define-type TEnv [Listof TBinding])
(define base-tenv
  (list
   (TBinding 'true (BoolT))
   (TBinding 'false (BoolT))
   (TBinding '+ (funT (list (NumT) (NumT)) (NumT)))
   (TBinding '- (funT (list (NumT) (NumT)) (NumT)))
   (TBinding '* (funT (list (NumT) (NumT)) (NumT)))
   (TBinding '/ (funT (list (NumT) (NumT)) (NumT)))
   (TBinding '<= (funT (list (NumT) (NumT)) (BoolT)))
   (TBinding 'num-eq? (funT (list (NumT) (NumT)) (BoolT)))
   (TBinding 'str-eq? (funT (list (StrT) (StrT)) (BoolT)))
   (TBinding 'substring (funT (list (StrT) (NumT) (NumT)) (StrT)))
   (TBinding 'strlen (funT (list (StrT)) (NumT)))))
(: extend-tenv (TBinding TEnv -> TEnv))
(define (extend-tenv b env)
  (cons b env))

(define-type Value (U NumV BoolV PrimV StrV CloV ArrayV NullV))
(struct NullV () #:transparent)
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct PrimV ([val : Symbol]) #:transparent) 
(struct StrV ([s : String]) #:transparent)
(struct CloV ([params : (Listof ParamC)] [body : ExprC] [env : Env]) #:transparent)  ;; changed to paramC from symbol
(struct ArrayV ([start : Integer] [size : Natural]) #:transparent)

(struct GivenBind ([ty : Type] [name : Symbol] [rhs : ExprC]) #:transparent)
(struct Binding ([name : Symbol] [val : Integer]) #:transparent)
(define-type Env [Listof Binding])
(define top-env (list
                 (Binding 'true 1)
                 (Binding 'false 2)
                 (Binding '+ 3)
                 (Binding '- 4)
                 (Binding '* 5)
                 (Binding '/ 6)
                 (Binding '<= 7)
                 (Binding 'num-eq? 8)
                 (Binding 'str-eq? 9)
                 (Binding 'substring 10)
                 (Binding 'strlen 11)
                 (Binding 'chain 12)
                 (Binding 'make-array 13)
                 (Binding 'array 14)
                 (Binding 'aref 15)
                 (Binding 'aset! 16)
                 (Binding ':= 17)))
(define mt-env '())
(define extend-env cons)

(define-type Store (Vectorof Value))
(define mt-store (vector))


;; Store: index 0 holds the next free location.
;; Indices 1-13 hold the pre-allocated top-level primitives.
;;creates and returns a new store with primitives pre-loaded
(define (top-store [memsize : Integer]) : Store
  (define sto : Store
    (make-vector memsize (NullV)))
  (define prims : (Listof Value)
    (list (BoolV #t)
          (BoolV #f)
          (PrimV '+)
          (PrimV '-)
          (PrimV '*)
          (PrimV '/)
          (PrimV '<=)
          (PrimV 'num-eq?)
          (PrimV 'str-eq?)
          (PrimV 'substring)
          (PrimV 'strlen)
          (PrimV 'chain)
          (PrimV 'make-array)
          (PrimV 'array)
          (PrimV 'aref)
          (PrimV 'aset!)
          (PrimV ':=)))
  (for ([i (in-naturals 1)]
        [v prims])
    (vector-set! sto i v))
  ; index 0 = next free slot (18, after indices 1-17)
  (vector-set! sto 0 (NumV 18))
  sto)
       
;;takes an s-expression and calles parser and interp
(define (top-interp [fun-sexps : Sexp]) : String
  (define parsed (parse fun-sexps))
  (type-check parsed base-tenv)
  (serialize (interp parsed top-env (top-store 2000))))
 
;;accepts any VEBG4 value and returns a string
(define (serialize [val : Value]) : String
  (match val
    [(NumV a) (~v a)]
    [(StrV a) a]
    [(PrimV a) "#<primop>"]
    [(BoolV a) (if a "true" "false")]
    [(CloV _ _ _) "#<procedure>"]
    [(ArrayV _ _) "#<array>"]
    [(NullV) "null"]))

;;interpretation evaluation for VEBG language
(define (interp [a : ExprC] [env : Env] [sto : Store]) : Value
  (match a
    [(NumC n) (NumV n)]
    [(StrC s) (StrV s)]
    [(idC i) (lookup i env sto)]
    [(IfC test thn else)
     (match (interp test env sto)
       [(BoolV #t) (interp thn env sto)]
       [(BoolV #f) (interp else env sto)]
       [other (error 'VEBG-interp "if test did not evaluate to a boolean: ~e" other)])]
    [(LamC params body) (CloV params body env)]
    [(RebC (idC i) arg) (define a (interp arg env sto))
                        (vector-set! sto (env-lookup i env) a)
                        (NullV)]
    [(ChainC exprs)
     (interp-chain exprs env sto)]
    [(RecC _ name rhs body)
     (define loc (allocate sto 1))
     (vector-set! sto loc (StrV "#<VEBG-uninitialized-rec>"))
     (define rec-env (extend-env (Binding name loc) env))
     (define rhs-val (interp rhs rec-env sto))
     (vector-set! sto loc rhs-val)
     (interp body rec-env sto)]
    [(appC fun args)
     (define f-val (interp fun env sto))
     (define evaluated-args (map
                             (lambda ([a : ExprC]) (interp a env sto))
                             args))
     (apply-val f-val evaluated-args sto)]))
 
;;parses concrete syntax into AST for the language to interpret
;;takes an S expression and returns an ExprC
(define (parse [prog : Sexp]): ExprC
  (match prog
    [(? real? n) (NumC n)] 
    [(? string? s) (StrC s)]
    [(list 'if tst thn els)
     (IfC (parse tst) (parse thn) (parse els))]
    [(list first ':= snd)  
      (RebC (parse first) (parse snd))]
    [(list 'fn (list params ...) '-> body)
     (define parsed-params (parse-params params))
     (if (check-duplicates (map ParamC-name parsed-params))
         (error 'VEBG-parse "function cannot have duplicate parameters: ~e"
                (map ParamC-name parsed-params))
         (LamC parsed-params (parse body)))]
    [(list 'given bindings 'do body)
     (define parsed-bindings (parse-given-bindings bindings))
     (appC (LamC (map (lambda ([b : GivenBind]) : ParamC
              (ParamC (GivenBind-ty b) (GivenBind-name b)))
            parsed-bindings)
       (parse body))
      (map GivenBind-rhs parsed-bindings))]
    [(list 'given bad-parts ...)
     (error 'VEBG-parse "given must look like {given {[id = expr] ...} do expr}, got: ~e" prog)]
    [(list 'chain first rest ...)
     (ChainC (map parse (cons first rest)))]
    [(list 'rec-given (list (list ty (? symbol? name) '= rhs)) 'do body)
     (cond
       [(reserved-id? name)
        (error 'VEBG-parse "reserved word used as rec-given name: ~e" name)]
       [else
        (RecC (parse-type ty) name (parse rhs) (parse body))])]
    [(list 'rec-given bad-parts ...)
     (error 'VEBG-parse
            "rec-given must look like {rec-given {[type id = expr]} do expr}, got: ~e"
            prog)]
    [(list fun args ...)  
     (appC (parse fun) (map parse args))]    
    [(? symbol? a)
     (if (reserved-id? a)
         (error 'VEBG-parse "invalid id, got ~e" a)
         (idC a))]
    [other (error 'VEBG-parse "expected valid syntax, got ~e" other)]))

;; calls interp on chained expressions
(: interp-chain ((Listof ExprC) Env Store -> Value))
(define (interp-chain exprs env sto)
  (match exprs
    ['() (error 'VEBG-interp "empty chain")]
    [(list last-expr) (interp last-expr env sto)]
    [(cons first-expr rest-exprs)
     (interp first-expr env sto)
     (interp-chain rest-exprs env sto)]))

;;takes an Sexp and returns a type
(: parse-type (Sexp -> Type))
(define (parse-type [s : Sexp]) : Type
  (match s
    ['num (NumT)]
    ['bool (BoolT)]
    ['str (StrT)]
    [(list args ... '-> ret)
     (funT (map parse-type (cast args (Listof Sexp)))
           (parse-type ret))]
    [other (error 'VEBG-parse-type "invalid type syntax: ~e" other)]))

;;takes a ExprC and type environment
;;returns the Type
;;errors otherwise
(: type-check (ExprC TEnv -> Type))
(define (type-check [expr : ExprC] [env : TEnv]) : Type
  (match expr
    [(NumC n) (NumT)]
    [(StrC s) (StrT)]
    [(idC s) (ty-lookup s env)]

    [(IfC tst thn els)
     (define tst-type (type-check tst env))
     (define thn-type (type-check thn env))
     (define els-type (type-check els env))
     (cond
       [(not (equal? tst-type (BoolT)))
        (error 'VEBG-type-check "if test must have type bool, got: ~e" tst-type)]
       [(not (equal? thn-type els-type))
        (error 'VEBG-type-check "if branches must have same type, got: ~e and ~e"
               thn-type els-type)]
       [else thn-type])]

    [(LamC params body)
     (define new-TEnv
       (append
        (map (lambda ([p : ParamC]) : TBinding
               (TBinding (ParamC-name p) (ParamC-ty p)))
             params)
        env))
     (funT (map ParamC-ty params)
           (type-check body new-TEnv))]

    [(appC fun args)
     (define fun-type (type-check fun env))
     (match fun-type
       [(funT expected-arg-types ret-type)
        (define actual-arg-types
          (map (lambda ([a : ExprC]) : Type
                 (type-check a env))
               args))
        (if (equal? expected-arg-types actual-arg-types)
            ret-type
            (error 'VEBG-type-check
                   "function argument type mismatch, expected ~e but got ~e"
                   expected-arg-types actual-arg-types))]
       [other
        (error 'VEBG-type-check "cannot apply non-function type: ~e" other)])]

    [(ChainC exprs)
     (type-check-chain exprs env)]

    [(RebC (idC name) rhs)
     (define old-type (ty-lookup name env))
     (define new-type (type-check rhs env))
     (if (equal? old-type new-type)
         old-type
         (error 'VEBG-type-check
                "assignment type mismatch for ~e, expected ~e but got ~e"
                name old-type new-type))]

    [(RecC declared-ty name rhs body)
     (define rec-env
       (extend-tenv (TBinding name declared-ty) env))
     (define rhs-ty (type-check rhs rec-env))
     (cond
       [(not (equal? rhs-ty declared-ty))
        (error 'VEBG-type-check
               "rec-given rhs type mismatch for ~e, expected ~e but got ~e"
               name declared-ty rhs-ty)]
       [else
        (type-check body rec-env)])]))

;; takes list of chained Listof ExrC and TEnv, and type-checks them
;; return the type of last in the chain
(: type-check-chain ((Listof ExprC) TEnv -> Type))
(define (type-check-chain [exprs : (Listof ExprC)] [env : TEnv]) : Type
  (match exprs
    ['() (error 'VEBG-type-check "empty chain")]
    [(list last-expr) (type-check last-expr env)]
    [(cons first-expr rest-exprs)
     (type-check first-expr env)
     (type-check-chain rest-exprs env)]))


;;takes a type and an environment
;;looks up the type in the env and returns type
(: ty-lookup (Symbol TEnv -> Type))
(define (ty-lookup [query : Symbol] [env : TEnv]) : Type
  (match env
    ['() (error 'VEBG-type-check "type not found: ~e" query)]
    [(cons (TBinding name type) rst)
     (cond
       [(equal? query name) type]
       [else (ty-lookup query rst)])]))

;;---interp helper  functions -------------------------------

;;takes a store and number of locations
;;mutates the store and returns the base location
(define (allocate [sto : Store] [locs : Real]) : Integer
  (define free (NumV-n (cast (vector-ref sto 0) NumV)))
  (cond
    [(= free (vector-length sto)) (error 'VEBG "out of memory")]
    [(> (+ free locs) (vector-length sto)) (error 'VEBG "not enough memory to allocate")]
    [else (vector-set! sto 0 (NumV (+ free locs)))
          (cast free Integer)]))
 
;;combines env-lookup and store-lookup
;;takes a symbol to lookup, an environment, and a store
;;returns a value bound to the location in the store
(define (lookup [query : Symbol] [env : Env] [sto : Store]) : Value
  (store-lookup (env-lookup query env) sto))

;;takes a symbol and an environment
;;returns the store location bound to the symbol
(define (env-lookup [query : Symbol] [env : Env]) : Integer
  (match env
    ['() (error 'VEBG-interp-lookup "name not found: ~e" query)]
    [(cons (Binding name loc) rst)
     (cond
       [(equal? query name) loc]
       [else (env-lookup query rst)])]))

;;takes a store locationa and a store
;;returns the value located at the store location
(define (store-lookup [loc : Integer] [sto : Store]) : Value (vector-ref sto loc))
 
;;takes a function value and list of arguments
;; evaluates the function with arguments and returns Value
(define (apply-val [fun-val : Value] [args : (Listof Value)] [store : Store]) : Value
  (match fun-val
    [(CloV params body env)
     (interp body (match-args params args env store) store)]
    [(PrimV val) (if (null? args) fun-val
                     (binop val args store))]
    [other (error 'VEBG-interp "cannot apply non-function: ~e" other)]))
 
;;takes a list of parameters, arguments, an environment, and a store
;;allocates and assigns args into store, binds parameters to args
;;returns extended environment
(: match-args ((Listof ParamC) (Listof Value) Env Store -> Env))
(define (match-args [params : (Listof ParamC)] [args : (Listof Value)] [env : Env] [store : Store]): Env
  (match* (params args) 
    [('() '()) env]
    [((cons (ParamC _ name) r1) (cons f2 r2))
     (define space (allocate store 1))
     (vector-set! store space f2)
     (extend-env (Binding name space)
                 (match-args r1 r2 env store))]
    [((cons f1 r1) _) (error 'VEBG-interp "input mismatch, missing argument(s): ~e" params)]
    [(_ (cons f2 r2))  (error 'VEBG-interp "input mismatch, too many argument(s): ~e" args)]))

;;takes a binary operator and two Values
;;performs operator on values and returns a Value
(define (binop op [args : (Listof Value)] [store : Store]) : Value
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
    [('num-eq? (list (NumV x) (NumV y)))
     (BoolV (= x y))]
    [('str-eq? (list (StrV x) (StrV y)))
     (BoolV (equal? x y))]
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
    [('chain progs)
     (chain-progs progs store)]
    [('make-array (list (NumV size) val))
     (cond
       [(< size 1) (error 'VEBG "cannot create array size <1: ~e" size)]
       [(not (exact-nonnegative-integer? size)) (error 'VEBG-make-array "size must be an integer: ~e" size)]
       [else (define start (allocate store size))
             (for ([i (in-range start (NumV-n (cast (vector-ref store 0) NumV)))])
               (vector-set! store i val))
             (ArrayV start (cast size Natural))])]
    [('array  elements) 
     (cond
       [(< (length elements) 1) (error 'VEBG "cannot create array size <1: ~e" (length elements))]
       [else (define array (ArrayV (allocate store (length elements)) (length elements)))
             (for ([i (in-naturals (ArrayV-start array))]
                   [e elements])
               (vector-set! store i e))
             array])]
    [('aref (list (ArrayV start size) (NumV index)))
     [cond 
       [(or (>= index (cast size Real)) (< index 0))
        (error 'VEBG-aref "array reference out of bounds: ~e" index)]
       [else (vector-ref store (cast (+ start index) Integer))]]]
    [('aset! (list (ArrayV start size) (NumV index) val))
     [cond  
       [(or (>= index (cast size Real)) (< index 0))
        (error 'VEBG-aref "array reference out of bounds: ~e" index)]
       [(not (exact-nonnegative-integer? index)) (error 'VEBG-aref "index must be an integer: ~e" index)]
       [else (vector-set! store (cast (+ start index) Integer) val)
             (NullV)]]]
             
    [(_ _) (error 'VEBG-binop "invalid binary operation: ~e ~e"
                  op args)]))

;;takes a list of expressions, evaluates each, and returns value of the last one
;;([params : (Listof Symbol)] [body : ExprC] [env : Env])
(define (chain-progs [progs : (Listof Value)] [store : Store]) : Value
  (match progs
    [(cons (CloV params body env) '()) 
     (interp body (match-args params '() env store) store)]
    [(cons (CloV params body env) rst)
     (interp body (match-args params '() env store) store)
     (chain-progs rst store)]
    [(list vals ...) (last vals)]))

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

;;takes an Sexp and returns a list of ParamC
(: parse-params (Sexp -> (Listof ParamC)))
(define (parse-params [params : Sexp]) : (Listof ParamC)
  (match params
    ['() '()]
    [(cons (list ty (? symbol? name)) rst)
     (cond
       [(reserved-id? name)
        (error 'VEBG-parse "reserved word used as parameter name: ~e" name)]
       [else
        (cons (ParamC (parse-type ty) name)
              (parse-params rst))])]
    [other
     (error 'VEBG-parse "params must look like ([type id] ...), got: ~e" other)]))

;;-----helper functions for given/parse-------------------------------

;;parses a single [id = expr] binding
(: parse-given-binding (Sexp -> GivenBind))
(define (parse-given-binding [b : Sexp]) : GivenBind
  (match b 
    [(list ty (? symbol? name) '= rhs)
     (cond
       [(reserved-id? name)
        (error 'VEBG-parse "reserved word used as given binding name: ~e" name)]
       [else
        (GivenBind (parse-type ty) name (parse rhs))])]
    [other
     (error 'VEBG-parse
            "given binding must look like [type id = expr], got: ~e"
            other)]))
 
;;parses a list of given bindings, checks for duplicates
(define (parse-given-bindings [raw : Sexp]) : (Listof GivenBind)
  (match raw
    [(list bindings ...)
     (define parsed
       (map (lambda ([b : Sexp]) : GivenBind
              (parse-given-binding b))
            (cast bindings (Listof Sexp))))
     (if (check-duplicates (map GivenBind-name parsed))
         (error 'VEBG-parse "duplicate given binding name: ~e" (map GivenBind-name parsed))
         parsed)]
    [other (error 'VEBG-parse "given must contain a list of bindings, got: ~e" other)]))

;; checks if the passed symbol is a reserved word
(define (reserved-id? a)
  (member a '(-> if fn given = do := : rec-given chain)))


;;---------------------tests-------------------------------------------- --------------------------------

;; parse-type tests
(check-equal? (parse-type 'num) (NumT))
(check-equal? (parse-type 'bool) (BoolT))
(check-equal? (parse-type 'str) (StrT))
(check-equal? (parse-type '{num -> num})
              (funT (list (NumT)) (NumT)))
(check-equal? (parse-type '{num str -> bool})
              (funT (list (NumT) (StrT)) (BoolT)))
(check-equal? (parse-type '{{num -> num} num -> num})
              (funT (list (funT (list (NumT)) (NumT)) (NumT)) (NumT)))
(check-exn #rx"VEBG-parse-type: invalid type syntax"
           (lambda () (parse-type 'array)))

;; parse-params tests
(check-equal? (parse-params '()) '())
(check-equal? (parse-params '([num x]))
              (list (ParamC (NumT) 'x)))
(check-equal? (parse-params '([num x] [bool b] [str s]))
              (list (ParamC (NumT) 'x)
                    (ParamC (BoolT) 'b)
                    (ParamC (StrT) 's)))
(check-equal? (parse-params '([{num -> num} f] [num x]))
              (list (ParamC (funT (list (NumT)) (NumT)) 'f)
                    (ParamC (NumT) 'x)))
(check-exn #rx"VEBG-parse: params must look like"
           (lambda () (parse-params '(x))))
(check-exn #rx"VEBG-parse-type: invalid type syntax"
           (lambda () (parse-params '([bad x]))))
(check-exn #rx"VEBG-parse: reserved word used as parameter name"
           (lambda () (parse-params '([num rec-given]))))

;; parse-given-binding tests
(check-equal? (parse-given-binding '[num x = 10])
              (GivenBind (NumT) 'x (NumC 10)))
(check-equal? (parse-given-binding '[bool b = true])
              (GivenBind (BoolT) 'b (idC 'true)))
(check-equal? (parse-given-binding '[str s = "hi"])
              (GivenBind (StrT) 's (StrC "hi")))
(check-equal? (parse-given-binding '[{num -> num} f = {fn ([num x]) -> x}])
              (GivenBind
               (funT (list (NumT)) (NumT))
               'f
               (LamC (list (ParamC (NumT) 'x)) (idC 'x))))
(check-exn #rx"VEBG-parse: reserved word used as given binding name"
           (lambda () (parse-given-binding '[num given = 10])))
(check-exn #rx"VEBG-parse: given binding must look like"
           (lambda () (parse-given-binding '[x = 10])))
(check-exn #rx"VEBG-parse: given binding must look like"
           (lambda () (parse-given-binding '[num x 10])))

;; parse-given-bindings tests
(check-equal? (parse-given-bindings '()) '())
(check-equal? (parse-given-bindings '([num x = 1] [str y = "hi"]))
              (list (GivenBind (NumT) 'x (NumC 1))
                    (GivenBind (StrT) 'y (StrC "hi"))))
(check-equal? (parse-given-bindings '([num x = 1] [str y = "a"] [bool z = false]))
              (list (GivenBind (NumT) 'x (NumC 1))
                    (GivenBind (StrT) 'y (StrC "a"))
                    (GivenBind (BoolT) 'z (idC 'false))))
(check-exn #rx"VEBG-parse: duplicate given binding name"
           (lambda () (parse-given-bindings '([num x = 1] [num x = 2]))))
(check-exn #rx"VEBG-parse: given must contain a list of bindings"
           (lambda () (parse-given-bindings 42)))

;; reserved-id? tests
(check-equal? (not (false? (reserved-id? 'if))) #t)
(check-equal? (not (false? (reserved-id? 'fn))) #t)
(check-equal? (not (false? (reserved-id? 'rec-given))) #t)
(check-equal? (not (false? (reserved-id? ':))) #t)
(check-equal? (reserved-id? 'x) #f)

;; parse tests
(check-equal? (parse 5) (NumC 5))
(check-equal? (parse "hello") (StrC "hello"))
(check-equal? (parse 'x) (idC 'x))
(check-equal? (parse '{+})
              (appC (idC '+) '()))
(check-equal? (parse '{+ 1 2})
              (appC (idC '+) (list (NumC 1) (NumC 2))))
(check-equal? (parse '{+ {* 2 3} {- 10 4}})
              (appC (idC '+)
                    (list
                     (appC (idC '*) (list (NumC 2) (NumC 3)))
                     (appC (idC '-) (list (NumC 10) (NumC 4))))))
(check-equal? (parse '{if true 1 2})
              (IfC (idC 'true) (NumC 1) (NumC 2)))
(check-equal? (parse '{x := 10})
              (RebC (idC 'x) (NumC 10)))
(check-equal? (parse '{chain 1 2 3})
              (ChainC (list (NumC 1) (NumC 2) (NumC 3))))
(check-equal? (parse '{chain 9})
              (ChainC (list (NumC 9))))
(check-equal? (parse '{fn ([num x]) -> x})
              (LamC (list (ParamC (NumT) 'x)) (idC 'x)))
(check-equal? (parse '{fn ([num x] [str y]) -> x})
              (LamC (list (ParamC (NumT) 'x)
                          (ParamC (StrT) 'y))
                    (idC 'x)))
(check-equal? (parse '{given {[num x = 5]} do x})
              (appC (LamC (list (ParamC (NumT) 'x)) (idC 'x))
                    (list (NumC 5))))
(check-equal? (RecC? (parse '{rec-given {[{num -> num} fact =
                                           {fn ([num n]) ->
                                             {if {<= n 0}
                                                 1
                                                 {* n {fact {- n 1}}}}}]}
                                  do
                                  {fact 5}}))
              #t)

;; parse error tests
(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse 'fn)))
(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse '->)))
(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse 'chain)))
(check-exn #rx"VEBG-parse: expected valid syntax"
           (lambda () (parse #t)))
(check-exn #rx"VEBG-parse: params must look like"
           (lambda () (parse '{fn (x) -> x})))
(check-exn #rx"VEBG-parse: function cannot have duplicate parameters"
           (lambda () (parse '{fn ([num x] [str x]) -> x})))
(check-exn #rx"VEBG-parse: reserved word used as parameter name"
           (lambda () (parse '{fn ([num if]) -> 1})))
(check-exn #rx"VEBG-parse: given must look like"
           (lambda () (parse '{given {[num x = 1]} x})))
(check-exn #rx"VEBG-parse: rec-given must look like"
           (lambda () (parse '{rec-given {[num x = 1] [num y = 2]} do x})))
(check-exn #rx"VEBG-parse: rec-given must look like"
           (lambda () (parse '{rec-given {[num 1 = 2]} do 3})))

;; type-check basic tests
(check-equal? (type-check (NumC 10) base-tenv) (NumT))
(check-equal? (type-check (StrC "hello") base-tenv) (StrT))
(check-equal? (type-check (idC 'true) base-tenv) (BoolT))
(check-equal? (type-check (idC 'false) base-tenv) (BoolT))
(check-equal? (type-check (idC '+) base-tenv)
              (funT (list (NumT) (NumT)) (NumT)))
(check-equal? (type-check (idC 'str-eq?) base-tenv)
              (funT (list (StrT) (StrT)) (BoolT)))
(check-exn #rx"VEBG-type-check: type not found"
           (lambda () (type-check (idC 'missing) base-tenv)))

;; type-check if tests
(check-equal? (type-check (parse '{if true 1 2}) base-tenv) (NumT))
(check-equal? (type-check (parse '{if false "yes" "no"}) base-tenv) (StrT))
(check-exn #rx"VEBG-type-check: if test must have type bool"
           (lambda () (type-check (parse '{if 1 2 3}) base-tenv)))
(check-exn #rx"VEBG-type-check: if branches must have same type"
           (lambda () (type-check (parse '{if true 1 "bad"}) base-tenv)))

;; type-check function tests
(check-equal? (type-check (parse '{fn ([num x]) -> {+ x 1}}) base-tenv)
              (funT (list (NumT)) (NumT)))
(check-equal? (type-check (parse '{fn ([str s]) -> {strlen s}}) base-tenv)
              (funT (list (StrT)) (NumT)))
(check-equal? (type-check (parse '{{fn ([num x]) -> {+ x 1}} 10}) base-tenv)
              (NumT))
(check-equal? (type-check (parse '{{fn ([str s]) -> {strlen s}} "hello"}) base-tenv)
              (NumT))
(check-equal? (type-check (parse '{{fn ([{num -> num} f]) -> {f 10}}
                                    {fn ([num x]) -> {+ x 1}}})
                          base-tenv)
              (NumT))
(check-exn #rx"VEBG-type-check: function argument type mismatch"
           (lambda () (type-check (parse '{{fn ([num x]) -> x} "bad"}) base-tenv)))
(check-exn #rx"VEBG-type-check: cannot apply non-function type"
           (lambda () (type-check (parse '{1 2}) base-tenv)))

;; type-check given tests
(check-equal? (type-check (parse '{given {[num x = 5]} do {+ x 1}}) base-tenv)
              (NumT))
(check-equal? (type-check (parse '{given {[str s = "hello"] [num n = 2]}
                                          do
                                          {substring s 0 n}})
                          base-tenv)
              (StrT))
(check-equal? (type-check (parse '{given {[{num num -> num} add =
                                            {fn ([num x] [num y]) -> {+ x y}}]
                                           [num a = 40]
                                           [num b = 2]}
                                          do
                                          {add a b}})
                          base-tenv)
              (NumT))
(check-exn #rx"VEBG-type-check: function argument type mismatch"
           (lambda () (type-check (parse '{given {[num x = "bad"]} do x}) base-tenv)))
(check-exn #rx"VEBG-type-check: type not found"
           (lambda () (type-check (parse '{given {[num x = 1] [num y = x]} do y}) base-tenv)))

;; type-check assignment tests
(check-equal? (type-check (parse '{given {[num x = 0]}
                                         do
                                         {x := 10}})
                          base-tenv)
              (NumT))
(check-exn #rx"VEBG-type-check: assignment type mismatch"
           (lambda () (type-check (parse '{given {[num x = 0]}
                                                 do
                                                 {x := "bad"}})
                                  base-tenv)))

;; type-check chain tests
(check-equal? (type-check (parse '{chain 1 2 3}) base-tenv) (NumT))
(check-equal? (type-check (parse '{chain {+ 1 2} "done"}) base-tenv) (StrT))
(check-exn #rx"VEBG-type-check: empty chain"
           (lambda () (type-check-chain '() base-tenv)))

;; type-check rec-given tests
(check-equal? (type-check
               (parse '{rec-given {[{num -> num} f =
                                     {fn ([num x]) -> x}]}
                                   do
                                   {f 10}})
               base-tenv)
              (NumT))
(check-equal? (type-check
               (parse '{rec-given {[{num -> num} fact =
                                     {fn ([num n]) ->
                                       {if {<= n 0}
                                           1
                                           {* n {fact {- n 1}}}}}]}
                                   do
                                   {fact 5}})
               base-tenv)
              (NumT))
(check-equal? (type-check
               (parse '{rec-given {[{num -> str} count =
                                     {fn ([num n]) ->
                                       {if {<= n 0}
                                           "done"
                                           {count {- n 1}}}}]}
                                   do
                                   {count 5}})
               base-tenv)
              (StrT))
(check-exn #rx"VEBG-type-check: rec-given rhs type mismatch"
           (lambda ()
             (type-check
              (parse '{rec-given {[{num -> num} f =
                                    {fn ([num x]) -> "bad"}]}
                                  do
                                  {f 10}})
              base-tenv)))

;; top-interp number tests
(check-equal? (top-interp '{+ 1 2}) "3")
(check-equal? (top-interp '{- 10 3}) "7")
(check-equal? (top-interp '{* 6 7}) "42")
(check-equal? (top-interp '{/ 8 2}) "4")
(check-equal? (top-interp '{<= 1 2}) "true")
(check-equal? (top-interp '{<= 2 2}) "true")
(check-equal? (top-interp '{<= 3 2}) "false")
(check-equal? (top-interp '{num-eq? 1 1}) "true")
(check-equal? (top-interp '{num-eq? 1 2}) "false")
(check-exn #rx"VEBG-binop: cannot divide by zero"
           (lambda () (top-interp '{/ 1 0})))

;; top-interp string tests
(check-equal? (top-interp '{str-eq? "a" "a"}) "true")
(check-equal? (top-interp '{str-eq? "a" "b"}) "false")
(check-equal? (top-interp '{strlen "hello"}) "5")
(check-equal? (top-interp '{strlen ""}) "0")
(check-equal? (top-interp '{substring "hello" 0 5}) "hello")
(check-equal? (top-interp '{substring "hello" 1 3}) "el")
(check-equal? (top-interp '{substring "hello" 0 0}) "")

;; top-interp if tests
(check-equal? (top-interp '{if true "yes" "no"}) "yes")
(check-equal? (top-interp '{if false "yes" "no"}) "no")
(check-equal? (top-interp '{if {<= 1 2} 10 20}) "10")
(check-equal? (top-interp '{if {num-eq? 1 2} "bad" "good"}) "good")

;; top-interp function tests
(check-equal? (top-interp '{fn ([num x]) -> {* x x}}) "#<procedure>")
(check-equal? (top-interp '{{fn ([num x]) -> {- 2 x}} 2}) "0")
(check-equal? (top-interp '{{fn ([num x] [num y]) -> {+ x y}} 2 3}) "5")
(check-equal? (top-interp '{{fn ([num x]) -> {{fn ([num y]) -> {+ x y}} 4}} 3}) "7")
(check-equal? (top-interp '{{fn ([{num -> num} h]) -> {h 8}}
                            {fn ([num x]) -> {+ x 1}}})
              "9")
(check-equal? (top-interp '{{fn ([{num -> num} f] [num x]) -> {f {f x}}}
                            {fn ([num y]) -> {+ y 10}}
                            5})
              "25")
(check-equal? (top-interp '{{{fn ([num x]) ->
                              {fn ([num y]) -> {+ x y}}}
                             10}
                            20})
              "30")

;; top-interp given tests
(check-equal? (top-interp '{given {[num x = 5]} do x}) "5")
(check-equal? (top-interp '{given {[num z = {+ 9 14}] [num y = 98]}
                                   do
                                   {+ z y}})
              "121")
(check-equal? (top-interp '{given {[num x = 10]}
                                   do
                                   {given {[num x = 1] [num y = x]}
                                          do
                                          y}})
              "10")
(check-equal? (top-interp '{given {[num x = 10]}
                                   do
                                   {given {[{num -> num} f = {fn ([num y]) -> {+ x y}}]}
                                          do
                                          {given {[num x = 100]}
                                                 do
                                                 {f 1}}}})
              "11")
(check-equal? (top-interp '{given {[num x = 100]}
                                   do
                                   {given {[{num -> num} f = {fn ([num y]) -> {+ x y}}]}
                                          do
                                          {given {[num x = 1]}
                                                 do
                                                 {f 5}}}})
              "105")
(check-equal? (top-interp '{given {[{num num -> num} add =
                                    {fn ([num x] [num y]) -> {+ x y}}]
                                   [num a = 40]
                                   [num b = 2]}
                                  do
                                  {add a b}})
              "42")
(check-equal? (top-interp '{given {[str s = "assignment"]}
                                   do
                                   {substring s 0 {strlen "assign"}}})
              "assign")

;; top-interp chain and assignment tests
(check-equal? (top-interp '{chain 1 2 3}) "3")
(check-equal? (top-interp '{chain {+ 1 2}
                                  {* 3 4}
                                  {- 20 5}})
              "15")
(check-equal? (top-interp '{chain {+ 1 2}
                                  {str-eq? "a" "a"}
                                  "done"})
              "done")
(check-equal? (top-interp '{given {[num x = 0]}
                                   do
                                   {chain {x := 10}
                                          x}})
              "10")
(check-equal? (top-interp '{given {[num x = 0]}
                                   do
                                   {chain {x := {+ x 1}}
                                          {x := {+ x 1}}
                                          {x := {+ x 1}}
                                          x}})
              "3")
(check-equal? (top-interp '{given {[num x = 1]}
                                   do
                                   {chain {x := {+ x 10}}
                                          {x := {* x 2}}
                                          x}})
              "22")
(check-equal? (top-interp '{given {[num x = 0]}
                                   do
                                   {given {[{num -> num} bump =
                                            {fn ([num n]) ->
                                              {chain {x := {+ x n}}
                                                     x}}]}
                                          do
                                          {chain {bump 5}
                                                 {bump 7}}}})
              "12")

;; top-interp rec-given tests
(check-equal? (top-interp
               '{rec-given {[{num -> num} fact =
                             {fn ([num n]) ->
                               {if {<= n 0}
                                   1
                                   {* n {fact {- n 1}}}}}]}
                           do
                           {fact 5}})
              "120")
(check-equal? (top-interp
               '{rec-given {[{num -> num} fact =
                             {fn ([num n]) ->
                               {if {<= n 0}
                                   1
                                   {* n {fact {- n 1}}}}}]}
                           do
                           {fact 6}})
              "720")
(check-equal? (top-interp
               '{rec-given {[{num -> num} sum-down =
                             {fn ([num n]) ->
                               {if {<= n 0}
                                   0
                                   {+ n {sum-down {- n 1}}}}}]}
                           do
                           {sum-down 5}})
              "15")
(check-equal? (top-interp
               '{rec-given {[{num -> num} fib =
                             {fn ([num n]) ->
                               {if {<= n 1}
                                   n
                                   {+ {fib {- n 1}}
                                      {fib {- n 2}}}}}]}
                           do
                           {fib 8}})
              "21")
(check-equal? (top-interp
               '{rec-given {[{num -> str} count =
                             {fn ([num n]) ->
                               {if {<= n 0}
                                   "done"
                                   {count {- n 1}}}}]}
                           do
                           {count 5}})
              "done")

;; top-interp type error tests
(check-exn #rx"VEBG-type-check"
           (lambda () (top-interp '{num-eq? 1 "1"})))
(check-exn #rx"VEBG-type-check"
           (lambda () (top-interp '{str-eq? "1" 1})))
(check-exn #rx"VEBG-type-check"
           (lambda () (top-interp '{+ 1 "bad"})))
(check-exn #rx"VEBG-type-check"
           (lambda () (top-interp '{if 0 1 2})))
(check-exn #rx"VEBG-type-check"
           (lambda () (top-interp '{if true 1 "bad"})))
(check-exn #rx"VEBG-type-check"
           (lambda () (top-interp '{equal? 1 1})))
(check-exn #rx"VEBG-type-check"
           (lambda () (top-interp '{error 1})))
(check-exn #rx"VEBG-type-check: function argument type mismatch"
           (lambda () (top-interp '{{fn ([num x]) -> x} "bad"})))
(check-exn #rx"VEBG-type-check: rec-given rhs type mismatch"
           (lambda () (top-interp '{rec-given {[{num -> num} f =
                                                {fn ([num x]) -> "bad"}]}
                                            do
                                            {f 10}})))
(check-exn #rx"VEBG-type-check: assignment type mismatch"
           (lambda () (top-interp '{given {[num x = 0]}
                                           do
                                           {x := "bad"}})))

;; serialize tests
(check-equal? (serialize (NumV 34)) "34")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (StrV "hello")) "hello")
(check-equal? (serialize (PrimV '+)) "#<primop>")
(check-equal? (serialize (CloV '() (NumC 1) mt-env)) "#<procedure>")
(check-equal? (serialize (ArrayV 18 2)) "#<array>")
(check-equal? (serialize (NullV)) "null")

;; interp helper tests
(check-equal? (interp (IfC (idC 'true) (NumC 1) (NumC 2))
                      top-env
                      (top-store 100))
              (NumV 1))
(check-equal? (interp (IfC (idC 'false) (NumC 1) (NumC 2))
                      top-env
                      (top-store 100))
              (NumV 2))
(check-exn #rx"VEBG-interp: if test did not evaluate to a boolean"
           (lambda () (interp (IfC (NumC 5) (NumC 1) (NumC 2))
                              top-env
                              (top-store 100))))
(check-exn #rx"VEBG-interp: cannot apply non-function"
           (lambda () (interp (appC (NumC 3) (list (NumC 4)))
                              top-env
                              (top-store 100))))
(check-exn #rx"VEBG-interp-lookup: name not found"
           (lambda () (lookup 'missing mt-env (top-store 100))))

;; chain helper tests
(check-exn #rx"VEBG-interp: empty chain"
           (lambda () (interp-chain '() top-env (top-store 100))))
(check-equal? (interp-chain (list (NumC 42)) top-env (top-store 100))
              (NumV 42))
(check-equal? (interp-chain (list (NumC 1) (NumC 2) (NumC 3))
                            top-env
                            (top-store 100))
              (NumV 3))

;; store helper tests
(check-equal? (vector-ref (top-store 100) 0) (NumV 18))
(check-exn #rx"VEBG: out of memory"
           (lambda ()
             (let ([sto (top-store 18)])
               (allocate sto 1))))
(check-exn #rx"VEBG: not enough memory to allocate"
           (lambda ()
             (let ([sto (top-store 20)])
               (allocate sto 5))))

;; binop number helper tests
(check-equal? (binop '+ (list (NumV 2) (NumV 3)) (top-store 100)) (NumV 5))
(check-equal? (binop '- (list (NumV 10) (NumV 4)) (top-store 100)) (NumV 6))
(check-equal? (binop '* (list (NumV 6) (NumV 7)) (top-store 100)) (NumV 42))
(check-equal? (binop '/ (list (NumV 9) (NumV 3)) (top-store 100)) (NumV 3))
(check-equal? (binop '<= (list (NumV 1) (NumV 2)) (top-store 100)) (BoolV #t))
(check-equal? (binop '<= (list (NumV 2) (NumV 2)) (top-store 100)) (BoolV #t))
(check-equal? (binop '<= (list (NumV 3) (NumV 2)) (top-store 100)) (BoolV #f))
(check-equal? (binop 'num-eq? (list (NumV 4) (NumV 4)) (top-store 100)) (BoolV #t))
(check-equal? (binop 'num-eq? (list (NumV 4) (NumV 5)) (top-store 100)) (BoolV #f))
(check-exn #rx"VEBG-binop: cannot divide by zero"
           (lambda () (binop '/ (list (NumV 1) (NumV 0)) (top-store 100))))
(check-exn #rx"VEBG-binop: invalid binary operation"
           (lambda () (binop '+ (list (NumV 1) (StrV "bad")) (top-store 100))))
(check-exn #rx"VEBG-binop: invalid binary operation"
           (lambda () (binop '+ (list (NumV 1)) (top-store 100))))
(check-exn #rx"VEBG-binop: invalid binary operation"
           (lambda () (binop 'num-eq? (list (NumV 1) (StrV "1")) (top-store 100))))
(check-exn #rx"VEBG-binop: invalid binary operation"
           (lambda () (binop 'wat (list (NumV 1) (NumV 2)) (top-store 100))))

;; binop string helper tests
(check-equal? (binop 'str-eq? (list (StrV "x") (StrV "x")) (top-store 100)) (BoolV #t))
(check-equal? (binop 'str-eq? (list (StrV "x") (StrV "y")) (top-store 100)) (BoolV #f))
(check-equal? (binop 'substring
                     (list (StrV "racecar") (NumV 0) (NumV 7))
                     (top-store 100))
              (StrV "racecar"))
(check-equal? (binop 'substring
                     (list (StrV "abcdef") (NumV 2) (NumV 5))
                     (top-store 100))
              (StrV "cde"))
(check-equal? (binop 'strlen
                     (list (StrV "racecar"))
                     (top-store 100))
              (NumV 7))
(check-exn #rx"VEBG-binop: invalid binary operation"
           (lambda () (binop 'str-eq? (list (StrV "1") (NumV 1)) (top-store 100))))
(check-exn #rx"VEBG-substring: first argument must be a string"
           (lambda () (binop 'substring
                             (list (NumV 5) (NumV 0) (NumV 1))
                             (top-store 100))))
(check-exn #rx"VEBG-substring: stop must be less than string length"
           (lambda () (binop 'substring
                             (list (StrV "hello") (NumV 0) (NumV 6))
                             (top-store 100))))
(check-exn #rx"VEBG-substring: start must be exact non-negative integer"
           (lambda () (binop 'substring
                             (list (StrV "hello") (NumV 1.5) (NumV 3))
                             (top-store 100))))
(check-exn #rx"VEBG-substring: start must be exact non-negative integer"
           (lambda () (binop 'substring
                             (list (StrV "hello") (NumV 1) (NumV 3.5))
                             (top-store 100))))
(check-exn #rx"VEBG-substring: stop must come after start"
           (lambda () (binop 'substring
                             (list (StrV "hello") (NumV 3) (NumV 1))
                             (top-store 100))))
(check-exn #rx"VEBG-strlen: input must be a string"
           (lambda () (binop 'strlen
                             (list (NumV 3))
                             (top-store 100))))

;; array helper tests
(let ([sto (top-store 100)])
  (check-equal? (binop 'array (list (NumV 1) (NumV 2)) sto)
                (ArrayV 18 2)))

(let* ([sto (top-store 100)]
       [arr (binop 'array (list (NumV 1) (NumV 2) (NumV 3)) sto)])
  (check-equal? (binop 'aref (list arr (NumV 0)) sto) (NumV 1))
  (check-equal? (binop 'aref (list arr (NumV 2)) sto) (NumV 3)))

(let* ([sto (top-store 100)]
       [arr (binop 'array (list (NumV 1) (NumV 2)) sto)])
  (check-equal? (binop 'aset! (list arr (NumV 1) (NumV 99)) sto)
                (NullV))
  (check-equal? (binop 'aref (list arr (NumV 1)) sto)
                (NumV 99)))

(check-exn #rx"VEBG: cannot create array size <1"
           (lambda () (binop 'make-array
                             (list (NumV 0) (NumV 0))
                             (top-store 100))))
(check-exn #rx"VEBG-make-array: size must be an integer"
           (lambda () (binop 'make-array
                             (list (NumV 2.1) (NumV 0))
                             (top-store 100))))
(check-exn #rx"VEBG: cannot create array size <1"
           (lambda () (binop 'array '() (top-store 100))))

(let* ([sto (top-store 100)]
       [arr (binop 'array (list (NumV 1) (NumV 2)) sto)])
  (check-exn #rx"VEBG-aref: array reference out of bounds"
             (lambda () (binop 'aref (list arr (NumV 2)) sto)))
  (check-exn #rx"VEBG-aref: array reference out of bounds"
             (lambda () (binop 'aset! (list arr (NumV -1) (NumV 99)) sto)))
  (check-exn #rx"VEBG-aref: index must be an integer"
             (lambda () (binop 'aset! (list arr (NumV 1.5) (NumV 99)) sto))))

;; match-args helper tests
(let ([sto (top-store 100)])
  (check-equal? (match-args (list (ParamC (NumT) 'a)
                                  (ParamC (NumT) 'b)
                                  (ParamC (NumT) 'c))
                            (list (NumV 1) (NumV 2) (NumV 3))
                            top-env
                            sto)
                (list (Binding 'a 18)
                      (Binding 'b 19)
                      (Binding 'c 20)
                      (Binding 'true 1)
                      (Binding 'false 2)
                      (Binding '+ 3)
                      (Binding '- 4)
                      (Binding '* 5)
                      (Binding '/ 6)
                      (Binding '<= 7)
                      (Binding 'num-eq? 8)
                      (Binding 'str-eq? 9)
                      (Binding 'substring 10)
                      (Binding 'strlen 11)
                      (Binding 'chain 12)
                      (Binding 'make-array 13)
                      (Binding 'array 14)
                      (Binding 'aref 15)
                      (Binding 'aset! 16)
                      (Binding ':= 17))))

(check-exn #rx"VEBG-interp: input mismatch, too many argument"
           (lambda () (apply-val
                       (CloV (list (ParamC (NumT) 'x)) (idC 'x) mt-env)
                       (list (NumV 1) (NumV 2))
                       (top-store 100))))
(check-exn #rx"VEBG-interp: input mismatch, missing argument"
           (lambda () (apply-val
                       (CloV (list (ParamC (NumT) 'x)
                                   (ParamC (NumT) 'y))
                             (idC 'x)
                             mt-env)
                       (list (NumV 1))
                       (top-store 100))))

;; ------------------------------------------------------------
;; Additional parse / syntax error tests
;; ------------------------------------------------------------

;; Covers the reserved-id? branch inside the syntactically valid rec-given parser clause.
(check-exn #rx"VEBG-parse: reserved word used as rec-given name"
           (lambda ()
             (parse '{rec-given {[num if = 1]}
                                do
                                if})))

;; Covers the parser path where `given` has the right outer shape,
;; but the bindings part is not a list of bindings.
(check-exn #rx"VEBG-parse: given must contain a list of bindings"
           (lambda ()
             (parse '{given 42 do 1})))

;; `:=` is reserved by your implementation, so it should not parse as an identifier.
(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse ':=)))

;; `do` is reserved and currently not directly checked in parse error tests.
(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse 'do)))

;; `given` is reserved and currently only tested through malformed given syntax,
;; not as a bare identifier.
(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse 'given)))

;; `rec-given` is reserved and currently only tested through malformed rec-given syntax,
;; not as a bare identifier.
(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse 'rec-given)))


;; chain-progs helper tests

(check-equal?
 (chain-progs
  (list (NumV 1) (NumV 2) (StrV "done"))
  (top-store 100))
 (StrV "done"))

(check-equal?
 (chain-progs
  (list (CloV '() (NumC 42) mt-env))
  (top-store 100))
 (NumV 42))

(check-equal?
 (chain-progs
  (list (CloV '() (NumC 1) mt-env)
        (CloV '() (NumC 2) mt-env)
        (NumV 99))
  (top-store 100))
 (NumV 99))

;; -- some tests for error raising
(check-exn #rx"VEBG-error: user-error"
           (lambda ()
             (binop 'error
                    (list (StrV "boom"))
                    (top-store 100))))

(check-exn #rx"VEBG-error: user-error"
           (lambda ()
             (binop 'error
                    (list (NumV 123))
                    (top-store 100))))

;; --- binpo tests
(check-equal?
 (binop 'chain
        (list (NumV 1) (NumV 2) (NumV 3))
        (top-store 100))
 (NumV 3))

(check-equal?
 (binop 'chain
        (list (StrV "first") (StrV "last"))
        (top-store 100))
 (StrV "last"))

(check-equal?
 (binop 'chain
        (list (CloV '() (NumC 11) mt-env)
              (CloV '() (NumC 22) mt-env))
        (top-store 100))
 (NumV 22))

;; make array
(let ([sto (top-store 100)])
  (check-equal?
   (binop 'make-array
          (list (NumV 3) (StrV "x"))
          sto)
   (ArrayV 18 3))

  (check-equal? (store-lookup 18 sto) (StrV "x"))
  (check-equal? (store-lookup 19 sto) (StrV "x"))
  (check-equal? (store-lookup 20 sto) (StrV "x"))
  (check-equal? (vector-ref sto 0) (NumV 21)))

;; apply-val
(check-equal?
 (apply-val (PrimV '+) '() (top-store 100))
 (PrimV '+))

(check-equal?
 (apply-val (PrimV '+)
            (list (NumV 20) (NumV 22))
            (top-store 100))
 (NumV 42))

(check-equal?
 (apply-val
  (CloV (list (ParamC (NumT) 'x))
        (idC 'x)
        mt-env)
  (list (NumV 55))
  (top-store 100))
 (NumV 55))

(check-exn #rx"VEBG-interp: cannot apply non-function"
           (lambda ()
             (apply-val (NumV 10)
                        (list (NumV 1))
                        (top-store 100))))