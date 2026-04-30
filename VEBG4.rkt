#lang typed/racket

(require typed/rackunit)
;;haven't implemented interp-fns yet
;;still using top-interp from lab 3

(define-type ExprC (U NumC BinOp idC appC Ifleq0C))
(struct NumC ([n : Real]) #:transparent)
(struct BinOp ([op : (U '+ '* '- '/)] [frst : ExprC] [snd : ExprC]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct appC ([fun : Symbol] [arg : (Listof ExprC)]) #:transparent)
(struct Ifleq0C ([test : ExprC] [thn : ExprC] [els : ExprC]) #:transparent)
(struct Binding ([name : Symbol] [val : Real]))
(define-type Env [Listof Binding])
(define mt-env '())
(define extend-env cons)

(struct FundefC ([name : Symbol] [arg : (Listof Symbol)] [body : ExprC]) #:transparent)

(define-type keywords (U '+ '- '* '/ 'named-fn '-> 'ifleq0?))

;;binop lookup
(define (BinopTable [op : Symbol])
  (match op
    ['+ +]
    ['- -]
    ['* *]))

;;takes a symbol and list of function definitions
;;returns function definition with given symbol
(define (get-fundef [s : Symbol] [fds : (Listof FundefC)]) : FundefC
  (cond
    [(empty? fds) (error 'VEBG-get-fundef "reference to undefined function")]
    [(cons? fds)
     (cond
       [(equal? s (FundefC-name (first fds))) (first fds)]
       [else (get-fundef s (rest fds))])]))



;;takes a symbol to lookup and an environment
;; returns a number to bind to the symbol
(define (lookup [query : Symbol] [env : Env]) : Real
  (match env
    ['() (error 'VEBG-lookup "name not found: ~e" query)]
    [(cons (Binding name val) rst)
     (cond
       [(symbol=? query name) val]
       [else (lookup query rst)])]))

;;takes a list of ExprCs (arguments) and symbols (parameters)
;;returns a list of bindings of arguments to parameters
(define (match-args [params : (Listof Symbol)] [args : (Listof Real)] [env : Env])
  : Env
  (match* (params args)
    [('() '()) env]
    [((cons f1 r1) (cons f2 r2)) (extend-env (Binding f1 f2)
                                       (match-args r1 r2 env))]
    [((cons f1 r1) '()) (error 'VEBG-interp "input mismatch, too many arguments: ~e" args )]
    [('() (cons f2 r2)) (error 'VEBG-interp "input mismatch, not enough arguments: ~e"  params)]))
                                                                
;; interpretation evaluation for VEBG language
(define (interp [a : ExprC] [fds : (Listof FundefC)] [env : Env]) : Real
  (match a
    [(NumC n) n]
    [(idC i) (lookup i env)]
    [(appC fun (list arg))
     (define fd (get-fundef fun fds)) 
     (interp (FundefC-body fd)
             fds
             (extend-env
              (Binding (first (FundefC-arg fd))
                    (interp arg fds env)) env))]
    [(appC fun (list args ...))
     (define fd (get-fundef fun fds))
     (define evaluated-args (map
                             (lambda ([a : ExprC]) (interp a fds env))
                             args))
     (interp (FundefC-body fd)
             fds
             (match-args (FundefC-arg fd) evaluated-args env))]
    [(BinOp o l r)
     (define l-val (interp l fds env))
     (define r-val (interp r fds env))
     (cond
       [(equal? o '/)
        (if (zero? r-val)
            (error 'VEBG-BinopTableDiv "cannot divide by zero")
            (/ l-val r-val))]
   [else
    ((BinopTable o) l-val r-val)])]
    [(Ifleq0C tst thn els)
     (if (<= (interp tst fds
                     mt-env) 0)
         (interp thn fds
                 mt-env)
         (interp els fds
                 mt-env))]))


(check-equal? (interp (appC 'f (list (NumC 1) (NumC 2)))
                      (list (FundefC 'f '(x y) (BinOp '+ (idC 'x) (idC 'y))))
                      mt-env)
              3)
                        
;;parse the given concret syntax into an AST
(define (parse [prog : Sexp]): ExprC
  (match prog
    [(? real? n) (NumC n)]
    [(list '+ left right) (BinOp '+ (parse left) (parse right))]
    [(list '* left right) (BinOp '* (parse left) (parse right))]
    [(list '/ left right) (BinOp '/ (parse left) (parse right))]
    [(list '- left right) (BinOp '- (parse left) (parse right))]
    [(list 'ifleq0? tst thn els)
     (Ifleq0C (parse tst) (parse thn) (parse els))]
    [(list (? symbol? fun) args ...)
     (if (member fun '(+ - * / ifleq0? named-fn ->))
     (error 'VEBG-parse "invalid function call ~e" prog)
     (appC fun (map parse args)))]
    [(? symbol? a) (if (or [eq? a '+]
                           [eq? a '-]
                           [eq? a '*]
                           [eq? a '/]
                           [eq? a '->]
                           [eq? a 'ifleq0?]
                           [eq? a 'named-fn])
                       (error 'VEBG-parse "invalid id, got ~e" a) (idC a))]
    [other (error 'VEBG-parse "expected valid syntax, got ~e" other)])) 


;;;helper for parse-fundef, below
(define (reserved-word? [s : Symbol]) : Boolean
  (or (symbol=? s '+)
      (symbol=? s '-)
      (symbol=? s '*)
      (symbol=? s '/)
      (symbol=? s 'named-fn)
      (symbol=? s '->)
      (symbol=? s 'ifleq0?)))

(define (has-duplicates? [xs : (Listof Symbol)]) : Boolean
  (cond
    [(empty? xs) #f]
    [(member (first xs) (rest xs)) #t]
    [else (has-duplicates? (rest xs))]))
;;parser for function definitions
;;takes an s expresison and returns a FundefC
(define (parse-fundef [func : Sexp]): FundefC
  (match func
    [(list 'named-fn (? symbol? name) (list (? symbol? arg) ...) '-> body)
     (cond
       [(reserved-word? name)
        (error 'VEBG-parse-fundef "invalid function name, got ~e" name)]
       [else
        (define params (cast arg (Listof Symbol)))
        (cond
          [(has-duplicates? params)
           (error 'VEBG-parse-fundef
              "duplicate parameter names in function ~e: ~e"
              name
              params)]
          [(ormap reserved-word? params)
           (error 'VEBG-parse-fundef
              "invalid parameter name in function ~e: ~e"
              name
              params)]
          [else
           (FundefC name params (parse body))])])]
    [other (error 'VEBG-parse-fundef "expected valid syntax:
{named-fn f (args) -> {body}}, got ~e" other)]))

;; parse-prog : Sexp -> (Listof FundefC)
(define (parse-prog [exp : Sexp]) : (Listof FundefC)
  (match exp
    [(list defs ...)
     (define parsed-defs (map parse-fundef defs))
     (define names (map FundefC-name parsed-defs))
     (if (has-duplicates? names)
         (error 'VEBG-parse-prog "duplicate function names in program: ~e" names)
         parsed-defs)]
    [other
     (error 'VEBG-parse-prog "VEBG: invalid program ~e" other)]))

;; interp-fns : (Listof FundefC) -> Real
;; finds main and evaluates it
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (define main-fn (get-fundef 'main funs))
  (cond
    [(not (empty? (FundefC-arg main-fn)))
     (error 'VEBG-interp-fns "main must have no arguments")]
    [else
     (interp (FundefC-body main-fn) funs mt-env)]))

;;takes an s-expression and calles parser and interp
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))

;;---tests------------------------------------------------------------------------------------------------

;;lookup test
(check-equal? (lookup 'x (list (Binding 'x 1)
                               (Binding 'y 2)))
              1)

;;get-fundef tests
(check-equal? (get-fundef 'target (list
                                   (FundefC 'a '(e) (idC 'f))
                                   (FundefC 'target '(arg) (NumC 2))))
              (FundefC 'target '(arg) (NumC 2)))

(check-exn #rx"VEBG-get-fundef: reference to undefined function" (lambda () (get-fundef 'a '())))

;;interp tests
(check-equal? (interp (appC 'add (list (NumC 5)))
                      (list (FundefC 'add '(x)
                                     (BinOp '+ (idC 'x) (appC 'sub (list (NumC 1)))))

                            (FundefC 'sub '(z) (BinOp '- (idC 'z) (appC 'mult (list (NumC 2)))))

                            (FundefC 'mult '(a) (BinOp '* (idC 'a) (appC 'div (list (NumC 1)))))

                            (FundefC 'div '(y) (BinOp '/ (idC 'y) (NumC 1))))
                      mt-env)
              4)
(check-exn #rx"VEBG-interp: unbound identifier error: 'y"
           (lambda () (interp (appC 'div (list (NumC 5)))
                              (list (FundefC 'div (list 'x) (BinOp '/ (idC 'y) (NumC 1))))
                              mt-env)))

;;ifleq tests
(check-equal?
 (interp (Ifleq0C (NumC 0) (NumC 1) (NumC 2)) '()
         mt-env)
 1)
(check-equal?
 (interp (Ifleq0C (NumC -4) (NumC 1) (NumC 2)) '()
         mt-env)
 1)
(check-equal?
 (interp (Ifleq0C (NumC 5) (NumC 1) (NumC 2)) '()
         mt-env)
 2)

;;parse-prog test
(check-equal? (parse-prog '{{named-fn f () -> 5}
                             {named-fn main () -> {+ {f} {f}}}})
              (list (FundefC 'f '() (NumC 5))
                    (FundefC 'main '() (BinOp '+ (appC 'f '()) (appC 'f '())))))
                                                               
;;top-interp tests
#;(check-equal? (top-interp '{{named-fn main () -> {* {+ 1 {- 10 {/ 1 1}}} 2}}}) 20)
(check-exn #rx"VEBG-BinopTableDiv: cannot divide by zero"
           (lambda () (top-interp '{{named-fn main () -> {/ 1 0}}})))

;;function parse tests
(check-equal? (parse-fundef '{named-fn double (x) -> (+ x x)})
              (FundefC 'double (list 'x) (BinOp '+ (idC 'x) (idC 'x))))
(check-equal? (parse-fundef '{named-fn f (x y) -> (+ x y)})
              (FundefC 'f '(x y) (BinOp '+ (idC 'x) (idC 'y))))
(check-exn #rx"VEBG-parse-fundef: expected valid syntax"
           (lambda () (parse-fundef '())))
(check-equal?
 (interp (appC 'dec-if-pos (list (NumC 5)))
         (list (FundefC 'dec-if-pos '(x)
                        (Ifleq0C (idC 'x)
                                 (idC 'x)
                                 (BinOp '- (idC 'x) (NumC 1)))))
         mt-env)
 4)

(check-equal?
 (interp (appC 'dec-if-pos (list (NumC -2)))
         (list (FundefC 'dec-if-pos  '(x)
                        (Ifleq0C (idC 'x)
                                 (idC 'x)
                                 (BinOp '- (idC 'x) (NumC 1)))))
         mt-env)
 -2)

(check-exn #rx"VEBG-parse-fundef: expected valid syntax"
           (lambda () (parse-fundef '())))

;;parse tests
(check-equal? (parse '(double 5)) (appC 'double (list (NumC 5))))
(check-equal? (parse '(f)) (appC 'f '()))
(check-exn #rx"VEBG-parse: invalid id, got '+" (lambda () (parse '+)))
(check-exn #rx"VEBG-parse: expected valid syntax, got '\\(1 a\\)" (lambda () (parse (list 1 'a))))
(check-exn #rx"VEBG-interp: input mismatch, not enough arguments: '\\(y\\)"
           (lambda () (match-args (list 'x 'y) (list 5) mt-env)))
(check-exn #rx"VEBG-interp: input mismatch, too many arguments: \\(list \\(NumC 5\\)\\)"
           (lambda () (match-args '() (list 5) mt-env)))

(check-equal?
 (parse '(ifleq0? 0 7 9))
 (Ifleq0C (NumC 0) (NumC 7) (NumC 9)))

(check-equal?
 (parse '(ifleq0? (- 3 5) 10 20))
 (Ifleq0C (BinOp '- (NumC 3) (NumC 5))
          (NumC 10)
          (NumC 20)))

;; interp + parse tests
(check-equal?
 (interp (parse '(ifleq0? 0 44 99)) '()
         mt-env)
 44)

(check-equal?
 (interp (parse '(ifleq0? 3 44 99)) '()
         mt-env)
 99)

(check-equal?
 (interp (parse '(ifleq0? (- 2 5) (+ 1 1) (+ 10 10))) '()
         mt-env)
 2)

(check-exn
 #rx"VEBG-parse-fundef: expected valid syntax:\n\\{named-fn f \\(args\\) -> \\{body\\}\\}, got"
 (λ ()
   (parse-fundef '())))
(check-exn
 #rx"VEBG-parse-fundef: expected valid syntax:\n\\{named-fn f \\(args\\) -> \\{body\\}\\}, got"
 (λ ()
   (parse-fundef '{named-fn f x -> x})))
(check-exn
 #rx"VEBG-parse-fundef: expected valid syntax:\n\\{named-fn f \\(args\\) -> \\{body\\}\\}, got"
 (λ ()
   (parse-fundef '/)))
(check-exn
 #rx"VEBG-BinopTableDiv: cannot divide by zero"
 (λ ()
   (top-interp
    '{{named-fn main () -> {/ 1 0}}})))

(check-exn
 #rx"VEBG-parse"
 (λ ()
   (parse '*)))
(check-exn
 #rx"VEBG-parse"
 (λ ()
   (parse '{+ 1 2 3})))
(check-exn
 #rx"VEBG-parse"
 (λ ()
   (parse '{ifleq0? 1 2})))

(check-exn
 #rx"VEBG-parse"
 (λ ()
   (parse '{named-fn 1 () -> 5})))
(check-exn
 #rx"VEBG-get-fundef"
 (λ ()
   (interp-fns
    (parse-prog
     '{{named-fn f () -> 5}}))))

(check-equal?
 (parse '{* 3 4})
 (BinOp '* (NumC 3) (NumC 4)))

(check-equal?
 (parse '{* {+ 1 2} 5})
 (BinOp '*
        (BinOp '+ (NumC 1) (NumC 2))
        (NumC 5)))

(check-exn
 #rx"VEBG-parse"
 (λ ()
   (parse '{* 1})))   ; missing right operand

(check-exn
 #rx"VEBG-parse-prog"
 (λ ()
   (parse-prog 42)))
(check-exn
 #rx"VEBG-interp-fns.*main must have no arguments"
 (λ ()
   (interp-fns
    (parse-prog
     '{{named-fn main (x) -> x}}))))

(check-exn
 #rx"VEBG-parse-fundef: duplicate parameter names"
 (λ ()
   (parse-fundef '{named-fn f (x x) -> (+ x x)})))

(check-exn
 #rx"VEBG-parse-fundef: invalid function name"
 (λ ()
   (parse-fundef '{named-fn + (x) -> x})))

(check-exn
 #rx"VEBG-parse-fundef: invalid parameter name"
 (λ ()
   (parse-fundef '{named-fn f (x ifleq0?) -> x})))

(check-exn
 #rx"VEBG-parse-prog: duplicate function names"
 (λ ()
   (parse-prog
    '{{named-fn f () -> 1}
      {named-fn f () -> 2}
      {named-fn main () -> 0}})))
