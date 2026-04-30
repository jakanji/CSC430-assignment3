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
(struct Binding ([name : Symbol] [val : Value]))
(define-type Env [Listof Binding])
(define mt-env '())
(define extend-env cons)
(define top-env (list (Binding 'true true) (Binding 'false false)))
(define-type Value (U Real Boolean))

(struct FundefC ([name : Symbol] [arg : (Listof Symbol)] [body : ExprC]) #:transparent)

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
(define (lookup [query : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'VEBG-lookup "name not found: ~e" query)]
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
    [((cons f1 r1) '()) (error 'VEBG-interp "input mismatch, too many arguments: ~e" args )]
    [('() (cons f2 r2)) (error 'VEBG-interp "input mismatch, not enough arguments: ~e"  params)]))

;;interpretation evaluation for VEBG language
(define (interp [a : ExprC] [fds : (Listof FundefC)] [env : Env]) : Value
  (match a
    [(NumC n) n]
    [(idC i) (lookup i env)]
    [(appC fun (list arg))
     (define fd (get-fundef fun fds)) 
     (interp (FundefC-body fd)
             fds
             (extend-env
              (Binding (first (FundefC-arg fd))
                    (interp arg fds env)) mt-env))]
    [(appC fun (list args ...))
     (define fd (get-fundef fun fds))
     (define evaluated-args (map
                             (lambda ([a : ExprC]) (interp a fds env))
                             args))
     (interp (FundefC-body fd)
             fds
             (match-args (FundefC-arg fd) evaluated-args mt-env))]
    [(BinOp o l r)
     (define l-val (interp l fds env))
     (define r-val (interp r fds env))
     (cond
       [(equal? o '/)
        (if (zero? (cast r-val Real))
            (error 'VEBG-BinopTableDiv "cannot divide by zero")
            (/ (cast l-val Real) (cast r-val Real)))]
   [else
    ((BinopTable o) (cast l-val Real) (cast r-val Real))])] 
    [(Ifleq0C tst thn els)
     (if (<= (cast (interp tst fds
                     mt-env) Real) 0)
         (interp thn fds
                 mt-env)
         (interp els fds
                 mt-env))]))

(check-equal? (interp (BinOp '+ (NumC 10) (appC 'const5 (list (NumC 10))))
                        (list (FundefC 'const5 '(_) (NumC 5)))
                        mt-env)
                15)
(check-equal? (interp (BinOp '+ (NumC 10) (appC 'double (list (BinOp '+ (NumC 1) (NumC 2)))))
                        (list (FundefC 'double '(x) (BinOp '+ (idC 'x) (idC 'x))))
                        mt-env)
                16)

(check-equal? (interp (BinOp '+ (NumC 10) (appC 'quadruple (list (BinOp '+ (NumC 1) (NumC 2)))))
                        (list (FundefC 'quadruple '(x) (appC 'double (list (appC 'double (list(idC 'x))))))
                              (FundefC 'double '(x) (BinOp '+ (idC 'x) (idC 'x))))
                        mt-env)
                22)

(check-equal? (interp (BinOp '+ (NumC 10) (appC 'double (list (BinOp '+ (NumC 1) (NumC 2)) (BinOp '+ (NumC 1) (NumC 2)))))
                        (list (FundefC 'double '(x y) (BinOp '+ (idC 'x) (idC 'y))))
                        mt-env)
                16)

             