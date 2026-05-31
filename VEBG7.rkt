#lang typed/racket

(require typed/rackunit)

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
(struct RecC ([name : Symbol] [rhs : ExprC] [body : ExprC]) #:transparent)

(define-type Type (U NumT BoolT StrT funT))
(struct NumT () #:transparent)
(struct BoolT () #:transparent)
(struct StrT () #:transparent)
(struct funT ([argT : (Listof Type)] [retT : Type]) #:transparent)

(struct TBinding ([id : Symbol] [ty : Type]) #:transparent)
(define-type TEnv [Listof TBinding])
(define base-tenv '())

(define-type Value (U NumV BoolV PrimV StrV CloV ArrayV NullV))
(struct NullV () #:transparent)
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct PrimV ([val : Symbol]) #:transparent) 
(struct StrV ([s : String]) #:transparent)
(struct CloV ([params : (Listof ParamC)] [body : ExprC] [env : Env]) #:transparent)  ;; changed to paramC from symbol
(struct ArrayV ([start : Integer] [size : Natural]) #:transparent)

(struct GivenBind ([name : Symbol] [rhs : ExprC]) #:transparent)
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
                 (Binding 'equal? 8)
                 (Binding 'substring 9)
                 (Binding 'strlen 10)
                 (Binding 'error 11)
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
          (PrimV 'equal?)
          (PrimV 'substring)
          (PrimV 'strlen)
          (PrimV 'error)
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
(: top-interp (Sexp -> String))
(define (top-interp fun-sexps)
  (serialize (interp (parse fun-sexps) top-env (top-store 2000))))
 
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
    [(RecC name rhs body)
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
     (appC (LamC (map GivenBind-name parsed-bindings) (parse body))
           (map GivenBind-rhs parsed-bindings))]
    [(list 'given bad-parts ...)
     (error 'VEBG-parse "given must look like {given {[id = expr] ...} do expr}, got: ~e" prog)]
    [(list 'chain first rest ...)
     (ChainC (map parse (cons first rest)))]
    [(list 'rec-given (list (list (? symbol? name) '= rhs)) 'do body)
     (cond
       [(not (LamC? (parse rhs)))
        (error 'VEBG-parse "rec-given rhs must be a function, got: ~e" rhs)]
       [(reserved-id? name)
        (error 'VEBG-parse "reserved word used as rec-given name: ~e" name)]
       [else
        (RecC name (parse rhs) (parse body))])]
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

;;takes a type and type environment
;;returns a type if type is correct
;;errors otherwise
(define (type-check [t : Type] [env : TEnv]) : Type
  (match t
    [(? number? n) n]
    [(? symbol? s) (ty-lookup s env)]
    [(IfT tst thn els) (if (boolean? tst)
                           (if (equal? thn els)
                               thn
                               (error 'VEBG-type-mismatch
                                      "if cases must have same type: ~e ~e" thn els))
                           (error 'VEBG-type-mismatch
                                  "test type must be boolean: ~e" tst))]))

;;takes a type and an environment
;;looks up the type in the env and returns type
(define (ty-lookup [t : Type] [env : TEnv]) : Type
  (match env
    ['() (error 'VEVG-type-check "type not found: ~e" t)]
    [(cons (TBinding name type) rst)
     (cond
       [(equal? t name) type]
       [else (ty-lookup t rst)])]))

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
    [('equal? (list x y)) (match* (x y)
                            [((NumV x) (NumV y)) (BoolV (= x y))]
                            [((StrV x) (StrV y)) (BoolV (equal? x y))]
                            [((BoolV x) (BoolV y)) (BoolV (equal? x y))]
                            [((ArrayV s1 v1) (ArrayV s2 v2)) (BoolV (equal? s1 s2))]
                            [(NullV NullV) (BoolV #t)]
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
(define (parse-given-binding [b : Sexp]) : GivenBind
  (match b 
    [(list (? symbol? name) '= rhs)
     (cond
       [(or (equal? name '->) (equal? name 'if) (equal? name 'fn)
            (equal? name 'given) (equal? name '=) (equal? name 'do) (equal? name ':=))
        (error 'VEBG-parse "reserved word used as given binding name: ~e" name)]
       [else (GivenBind name (parse rhs))])]
    [other (error 'VEBG-parse "given binding must look like [id = expr], got: ~e" other)]))
 
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

;;----end helper functions for parse-------------------------------
#;(
(define while
  '{given ([while = "placeholder"])
          do
          {chain {while := {fn (guard body) ->
                           {if {guard} 
                               {chain
                                {body}
                                {while guard body}}
                               false}}}  
                 while}})
   
(define in-order
  `(fn (arr size) ->
     {given ([i = 0]
             [increasing = true])
            do
            {chain {while {fn () -> {<= {+ i 1} {- size 1}}}
                          {fn () -> {if {<= {aref arr i} {aref arr {+ i 1}}}
                                        {i := {+ i 1}}
                                        {chain {increasing := false}
                                               {i := size}}}}}
                   increasing}})) 

(check-equal? (top-interp `{given ([while = ,while])
                      do
                      {given ([in-order = ,in-order]) 
                             do {in-order (array 1 2 3) 3}}}) "true")

;;---------------------tests-------------------------------------------- --------------------------------
 
;;factorial test
(check-equal? (top-interp
   '{given ([fact = "placeholder"])
           do
           {given ([f = {fn (n) -> {if {<= n 0}
                                       1
                                       {* n {fact {- n 1}}}}}])
                  do
                  {chain {fact := f}
                         {fact 6}}}}) "720")
 
(check-exn #rx"VEBG-aref: index must be an integer: 2.3" (lambda () 
           (top-interp '(given ((f = (make-array 5 false))) do (aset! f 2.3 19)))))
 
(check-equal? (top-interp
   '{given ([arr = {array 0}])
           do
           {given ([f = {fn () -> {aset! arr 0 {+ 1
                                                  {aref arr 0}}}}])
                  do 
                  {chain {f} {f} {f} {f}
                         {aref arr 0}}}}) "4")
 
(check-equal? (top-interp '{make-array 2
                                       2}) "#<array>")
 
(check-equal? (match-args '(a b c)
                          (list (NumV 1) (NumV 2) (NumV 3))
                          top-env
                          (top-store 100)) (list 
                                        (Binding 'a 18)
                                        (Binding 'b 19)
                                        (Binding 'c 20) 
                                        (Binding 'true 1)
                                        (Binding 'false 2)
                                        (Binding '+ 3)
                                        (Binding '- 4)
                                        (Binding '* 5)
                                        (Binding '/ 6)
                                        (Binding '<= 7)
                                        (Binding 'equal? 8)
                                        (Binding 'substring 9)
                                        (Binding 'strlen 10)
                                        (Binding 'error 11)
                                        (Binding 'chain 12)
                                        (Binding 'make-array 13)
                                        (Binding 'array 14)
                                        (Binding 'aref 15)
                                        (Binding 'aset! 16)
                                        (Binding ':= 17)))
(check-exn #rx"VEBG-make-array: size must be an integer: 2.1" (lambda ()
           (top-interp '(given ((f = (make-array 2.1 false))) do (aset! f 1 19)))))
(check-exn #rx"VEBG-parse: invalid id, got ':=" (lambda () (parse '(:= true false null))))
 
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
 (interp (IfC (idC 'true) (NumC 1) (NumC 2)) top-env (top-store 100))
 (NumV 1))
(check-equal?
 (interp (IfC (idC 'false) (NumC 1) (NumC 2)) top-env (top-store 100))
 (NumV 2))
(check-equal?
 (interp (IfC (appC (idC '<=) (list (NumC 0) (NumC 5))) (NumC 1) (NumC 2)) top-env (top-store 100))
 (NumV 1))
(check-equal?
 (interp (IfC (appC (idC '<=) (list (NumC 5) (NumC 0))) (NumC 1) (NumC 2)) top-env (top-store 100))
 (NumV 2))
;; if via top-interp
(check-equal? (top-interp '{if true "yes" "no"}) "yes")
(check-equal? (top-interp '{if false "yes" "no"}) "no")
(check-equal? (top-interp '{if {<= 1 2} 10 20}) "10")
;; non-boolean test should error
(check-exn #rx"VEBG-interp: if test did not evaluate to a boolean"
           (lambda () (interp (IfC (NumC 5) (NumC 1) (NumC 2)) top-env (top-store 100))))
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
 
;;serialize tests
(check-equal? (serialize (NumV 34)) "34")
(check-equal? (serialize (BoolV true)) "true")
(check-equal? (serialize (BoolV false)) "false")
 
;;serialize tests
(check-equal? (serialize (NumV 34)) "34")
(check-equal? (serialize (BoolV true)) "true")
(check-equal? (serialize (BoolV false)) "false")
 
;;interp tests
#;(check-equal? (interp (idC '+) top-env (top-store 100)) "+")
(check-equal? (interp (appC (idC '+) (list (appC (LamC '(x y)
                                                       (appC (idC '-) (list (idC 'x) (idC 'y))))
                                                 (list (NumC 2) (NumC 5)))
                                           (appC (LamC '(x)
                                                       (appC (idC '/)
                                                             (list (appC (idC '*)
                                                                         (list (NumC 2) (idC 'x)))
                                                                   (NumC 2))))
                                                 (list (NumC 10)))))
                      top-env (top-store 100))
              (NumV 7))
 
 
(check-equal? (interp (appC (idC '+) (list (NumC 10)
                                           (appC (LamC '(x y)
                                                       (appC (idC '*) (list (idC 'x) (idC 'y))))
                                                 (list (NumC 1) (NumC 2)))))
                      top-env (top-store 100))
              (NumV 12))
(check-exn #rx"VEBG-interp: cannot apply non-function"
           (lambda () (interp (appC (NumC 3) (list (NumC 4))) top-env (top-store 100))))
 
(check-exn #rx"VEBG-interp: input mismatch, too many argument\\(s\\): \\(list \\(NumV 3\\)\\)"
           (lambda () (top-interp '{{fn (x) -> (* x 2)} 2 3})))
(check-exn #rx"VEBG-interp: input mismatch, missing argument\\(s\\): '\\(z\\)"
           (lambda () (top-interp '{{fn (x y z) -> (* x 2)} 2 3})))
 
(check-exn #rx"VEBG-interp: cannot apply non-function: \\(NumV 7\\)"
           (lambda () (apply-val (NumV 7) '() (top-store 100))))
 
(check-exn #rx"VEBG-interp: cannot apply non-function"
           (lambda () (interp (appC (NumC 3) (list (NumC 4))) mt-env (top-store 100))))
 
(check-exn #rx"VEBG-interp-lookup: name not found: 'missing"
           (lambda () (lookup 'missing mt-env (top-store 100))))
 
;;interp - StrC
(check-equal? (interp (StrC "hello") mt-env (top-store 100)) (StrV "hello"))
 
;;serialize - StrV
(check-equal? (serialize (StrV "hello")) "hello")
 
;;binop - equal? on NumV, StrV, BoolV
(check-equal? (binop 'equal? (list (NumV 3) (NumV 3)) (top-store 100)) (BoolV #t))
(check-equal? (binop 'equal? (list (NumV 3) (NumV 4)) (top-store 100)) (BoolV #f))
(check-equal? (binop 'equal? (list (StrV "a") (StrV "a")) (top-store 100)) (BoolV #t))
(check-equal? (binop 'equal? (list (StrV "a") (StrV "b")) (top-store 100)) (BoolV #f))
(check-equal? (binop 'equal? (list (BoolV #t) (BoolV #t)) (top-store 100)) (BoolV #t))
(check-equal? (binop 'equal? (list (BoolV #t) (BoolV #f)) (top-store 100)) (BoolV #f))
 
;;parse - string literal
(check-equal? (parse '"hello") (StrC "hello"))
;;parse - valid symbol becomes idC
(check-equal? (parse 'x) (idC 'x))
;;parse - -> is a reserved word
(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse '->)))
  
;;substring tests
(check-equal? (top-interp '{substring "hello" 0 5}) "hello")
(check-equal? (top-interp '{substring "hello" 1 3}) "el")
(check-equal? (top-interp '{substring "hello" 0 0}) "")
;; direct binop calls
(check-equal? (binop 'substring (list (StrV "racecar") (NumV 0) (NumV 7)) (top-store 100)) (StrV "racecar"))
(check-equal? (binop 'substring (list (StrV "racecar") (NumV 3) (NumV 6)) (top-store 100)) (StrV "eca"))
;; stop > string length
(check-exn #rx"VEBG-substring: stop must be less than string length"
           (lambda () (binop 'substring (list (StrV "hello") (NumV 0) (NumV 6)) (top-store 100))))
;; non-integer start
(check-exn #rx"VEBG-substring: start must be exact non-negative integer: 1.5"
           (lambda () (binop 'substring (list (StrV "hello") (NumV 1.5) (NumV 3)) (top-store 100))))
;; non-integer stop (same message as start)
(check-exn #rx"VEBG-substring: start must be exact non-negative integer: 3.5"
           (lambda () (binop 'substring (list (StrV "hello") (NumV 1) (NumV 3.5)) (top-store 100))))
;; stop before start
(check-exn #rx"VEBG-substring: stop must come after start"
           (lambda () (binop 'substring (list (StrV "hello") (NumV 3) (NumV 1)) (top-store 100))))
;; non-string first argument
(check-exn #rx"VEBG-substring: first argument must be a string"
           (lambda () (binop 'substring (list (NumV 5) (NumV 0) (NumV 2)) (top-store 100))))
 
 
;;strlen tests
(check-equal? (top-interp '{strlen "hello"}) "5")
(check-equal? (top-interp '{strlen ""}) "0")
(check-equal? (binop 'strlen (list (StrV "racecar")) (top-store 100)) (NumV 7))
(check-exn #rx"VEBG-strlen: input must be a string: \\(NumV 3\\)"
           (lambda () (binop 'strlen (list (NumV 3)) (top-store 100))))
 
 
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

;; rec-given tests
;; simple
(check-equal?
 (top-interp
  '{rec-given {[fact = {fn (n) ->
                         {if {<= n 0}
                             1
                             {* n {fact {- n 1}}}}}]}
              do
              {fact 6}})
 "720")
;; uncategorised top-interp tests--------------
(check-equal?
 (top-interp '{chain 1 2 3})
 "3")

(check-equal?
 (top-interp '{chain {+ 1 2}
                     {* 3 4}
                     {- 20 5}})
 "15")

(check-equal?
 (top-interp
  '{given {[x = 0]}
          do
          {chain {x := 10}
                 x}})
 "10")

(check-equal?
 (top-interp
  '{given {[x = 0]}
          do
          {chain {x := {+ x 1}}
                 {x := {+ x 1}}
                 {x := {+ x 1}}
                 x}})
 "3")

(check-equal?
 (top-interp
  '{given {[arr = {array 0}]}
          do
          {chain {aset! arr 0 99}
                 {aref arr 0}}})
 "99")

;; ---------------- chain helper tests ----------------

(check-exn #rx"VEBG-interp: empty chain"
           (lambda ()
             (interp-chain '() top-env (top-store 100))))

(check-equal?
 (interp-chain (list (NumC 42)) top-env (top-store 100))
 (NumV 42))

(check-equal?
 (interp-chain (list (NumC 1)
                     (NumC 2)
                     (NumC 3))
               top-env
               (top-store 100))
 (NumV 3))
;; ----- Chain parse -------
(check-equal?
 (parse '{chain 1 2 3})
 (ChainC (list (NumC 1) (NumC 2) (NumC 3))))

(check-equal?
 (parse '{chain {+ 1 2} {* 3 4}})
 (ChainC
  (list
   (appC (idC '+) (list (NumC 1) (NumC 2)))
   (appC (idC '*) (list (NumC 3) (NumC 4))))))

(check-exn #rx"VEBG-parse: invalid id"
           (lambda () (parse 'chain)))


;; parse-given-binding - valid binding
(check-equal? (parse-given-binding '[x = 10])
              (GivenBind 'x (NumC 10)))
;; parse-given-binding - reserved word as name
(check-exn #rx"VEBG-parse: reserved word used as given binding name"
           (lambda () (parse-given-binding '[if = 10] )))
;; parse-given-binding - malformed binding
(check-exn #rx"VEBG-parse: given binding must look like \\[id = expr\\]"
           (lambda () (parse-given-binding '[x 10] )))
;; parse-given-bindings - duplicate names
(check-exn #rx"VEBG-parse: duplicate given binding name"
           (lambda () (parse-given-bindings '([x = 1] [x = 2]) )))
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
           (lambda () (parse-given-bindings 42 )))
 
(check-equal? (top-interp '{equal? 1 "1"}) "false")
;;------ new tests for uncovered lines ------
 
;; line 81: top-store sets free pointer to 18 (17 prims pre-loaded at indices 1-17)
(check-equal? (vector-ref (top-store 100) 0) (NumV 18))
 
;; line 98: NullV serializes to "null"
(check-equal? (serialize (NullV)) "null")
  
;; line 173: allocate - store completely full (free == vector-length)
(check-exn #rx"VEBG: out of memory"
           (lambda ()
             (let ([sto (top-store 18)])  ;; size 18: indices 0-17 all used
               (allocate sto 1))))
 
;; lines 174-175: allocate - not enough contiguous space
(check-exn #rx"VEBG: not enough memory to allocate"
           (lambda ()
             (let ([sto (top-store 20)])  ;; 2 slots free (18,19)
               (allocate sto 5))))        ;; request 5
 
;; line 242: equal? on ArrayV - same array is true, different arrays are false
(let* ([sto (top-store 100)]
       [a (cast (binop 'array (list (NumV 1) (NumV 2)) sto) ArrayV)]
       [b (cast (binop 'array (list (NumV 1) (NumV 2)) sto) ArrayV)])
  (check-equal? (binop 'equal? (list a a) sto) (BoolV #t))
  (check-equal? (binop 'equal? (list a b) sto) (BoolV #f)))
 
;; line 243: equal? on two NullV
(check-equal? (binop 'equal? (list (NullV) (NullV)) (top-store 100)) (BoolV #t))
 
;; line 259: make-array with size < 1
(check-exn #rx"VEBG: cannot create array size <1"
           (lambda () (binop 'make-array (list (NumV 0) (NumV 0)) (top-store 100))))
(check-exn #rx"VEBG: cannot create array size <1"
           (lambda () (binop 'make-array (list (NumV -1) (NumV 0)) (top-store 100))))
 
;; line 263: array with empty element list
(check-exn #rx"VEBG: cannot create array size <1"
           (lambda () (binop 'array '() (top-store 100))))
 
;; line 272: aref out of bounds (>= size and < 0)
(let* ([sto (top-store 100)]
       [arr (binop 'array (list (NumV 1) (NumV 2) (NumV 3)) sto)])
  (check-exn #rx"VEBG-aref: array reference out of bounds: 3"
             (lambda () (binop 'aref (list arr (NumV 3)) sto)))
  (check-exn #rx"VEBG-aref: array reference out of bounds: -1"
             (lambda () (binop 'aref (list arr (NumV -1)) sto))))
 
;; line 277: aset! out of bounds
(let* ([sto (top-store 100)]
       [arr (binop 'array (list (NumV 1) (NumV 2)) sto)])
  (check-exn #rx"VEBG-aref: array reference out of bounds: 2"
             (lambda () (binop 'aset! (list arr (NumV 2) (NumV 99)) sto)))
  (check-exn #rx"VEBG-aref: array reference out of bounds: -1"
             (lambda () (binop 'aset! (list arr (NumV -1) (NumV 99)) sto))))
 
;; lines 288-289: chain-progs single CloV thunk
(let ([sto (top-store 100)])
  (check-equal? (chain-progs (list (CloV '() (NumC 42) mt-env)) sto) (NumV 42)))
 
;; lines 290-292: chain-progs multiple CloV thunks - returns last
(let ([sto (top-store 100)])
  (check-equal? (chain-progs (list (CloV '() (NumC 1) mt-env)
                                   (CloV '() (NumC 2) mt-env)
                                   (CloV '() (NumC 3) mt-env)) sto)
                (NumV 3)))
)







;; -- parse-type tests
(check-equal? (parse-type 'num) (NumT))
(check-equal? (parse-type 'bool) (BoolT))
(check-equal? (parse-type 'str) (StrT))
(check-equal? (parse-type '{num str -> bool})
              (funT (list (NumT) (StrT)) (BoolT)))