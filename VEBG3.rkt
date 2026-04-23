#lang typed/racket
(require typed/rackunit)

(define-type ExprC (U NumC BinOp idC appC))
(struct NumC ([n : Real]) #:transparent)
(struct BinOp ([op : (U '+ '* '- '/)] [frst : ExprC] [snd : ExprC]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct appC ([fun : Symbol] [arg : ExprC]) #:transparent)

(struct FundefC ([name : Symbol] [arg : Symbol] [body : ExprC]) #:transparent)

;;binop lookup
(define (BinopTable [op : Symbol])
  (match op
    ['+ +]
    ['- -]
    ['* *]))
(define (BinopTableDiv [divisor : ExprC])
  (match divisor
    [(NumC 0) (error 'VEBG3-BinopTableDiv "cannot divide by zero")]
    [_ /]))

;;takes a symbol and list of function definitions
;;returns function definition with given symbol
(define (get-fundef [s : Symbol] [fds : (Listof FundefC)]) : FundefC
  (cond
    [(empty? fds) (error 'VEBG-get-fundef "reference to undefined function")]
    [(cons? fds)
     (cond
       [(equal? s (FundefC-name (first fds))) (first fds)]
       [else (get-fundef s (rest fds))])]))

;;takes two ExprCs (what to replace the name with and the experssion to do sub in)
;; and a symbol (name to replace) returns an ExprC
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(NumC n) in]
    [(idC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(appC f a) (appC f (subst what for a))]
    [(BinOp s l r) (cond
                     [(equal? s '/) (BinOp '/ (subst what for l) (subst what for r))]
                     [(equal? s '+) (BinOp '+ (subst what for l) (subst what for r))]
                     [(equal? s '*) (BinOp '* (subst what for l) (subst what for r))]
                     [(equal? s '-) (BinOp '- (subst what for l) (subst what for r))])]))

;; interpretation evaluation for VEBG language
(define (interp [a : ExprC] [fds : (Listof FundefC)]) : Real
  (match a
    [(NumC n) n]
    [(idC i) (error 'VEBG3-interp "unbound identifier error: ~e" i)]
    [(appC fun arg) (define fd (get-fundef fun fds))
                    (interp (subst arg
                                   (FundefC-arg fd)
                                   (FundefC-body fd)) fds)]
    [(BinOp o l r) (cond
                     [(equal? o '/) ((BinopTableDiv r) (interp l fds) (interp r fds))]
                     [else ((BinopTable o) (interp l fds) (interp r fds))])]))
                              
                        
;;parse the given concret syntax into an AST
(define (parse [prog : Sexp]): ExprC
  (match prog
    [(? real? n) (NumC n)]
    [(list '+ left right) (BinOp '+ (parse left) (parse right))]
    [(list '* left right) (BinOp '* (parse left) (parse right))]
    [(list '/ left right) (BinOp '/ (parse left) (parse right))]
    [(list '- left right) (BinOp '- (parse left) (parse right))]
    [(list (? symbol? fun) arg) (appC fun (parse arg))] 
    [(? symbol? a) (idC a)]
    [other (error 'VEBG3-parse "expected valid syntax, got ~e" other)]))

;;parser for function definitions
;;takes an s expresison and returns a FundefC
(define (parse-fundef [func : Sexp]): FundefC
  (match func
    [(list (? symbol? name) (? symbol? arg) body) (FundefC name arg (parse body))]
    [other (error 'VEBG3-parse-fundef "expected valid syntax, got ~e" other)]))

;;takes an s expression and returns a list of FundefCs

;;takes an s-expression and calles parser and interp
(define (top-interp [expr : Sexp]) : Real
  (interp
   (parse expr)
   (list (FundefC 'a 'b (idC 'c)))))

;;---tests------------------------------------------------------------------------------------------------

;;get-fundef tests
(check-equal? (get-fundef 'target (list
                                   (FundefC 'a 'e (idC 'f))
                                   (FundefC 'target 'arg (NumC 2))))
              (FundefC 'target 'arg (NumC 2)))
(check-exn #rx"VEBG-get-fundef: reference to undefined function" (lambda () (get-fundef 'a '())))

;;interp tests
(check-equal? (interp (appC 'double (NumC 5)) (list (FundefC 'double 'x (BinOp '* (idC 'x) (NumC 2)))))
              10)
(check-equal? (interp (appC 'add (NumC 5)) (list (FundefC 'add 'x (BinOp '+ (idC 'x) (NumC 2)))))
              7)
(check-equal? (interp (appC 'subtract (NumC 5)) (list (FundefC 'subtract 'x (BinOp '- (idC 'x) (NumC 2)))))
              3)
(check-equal? (interp (appC 'div (NumC 5)) (list (FundefC 'div 'x (BinOp '/ (idC 'x) (NumC 1)))))
              5)
(check-equal? (interp (appC 'add (NumC 5)) (list (FundefC 'add 'x (BinOp '+ (idC 'x) (NumC 2)))))
              7)
(check-exn #rx"VEBG3-interp: unbound identifier error: 'y"
           (lambda () (interp (appC 'div (NumC 5))
                              (list (FundefC 'div 'x (BinOp '/ (idC 'y) (NumC 1)))))))
                                                               
;;top-interp tests
(check-equal? (top-interp '(* (+ 1 (- 10 (/ 1 1))) 2))20)
(check-exn #rx"VEBG3-BinopTableDiv: cannot divide by zero" (lambda () (top-interp '(/ 1 0))))

;;function parse tests
(check-equal? (parse-fundef '(double x (+ x x)))
              (FundefC 'double 'x (BinOp '+ (idC 'x) (idC 'x))))
(check-exn #rx"VEBG3-parse-fundef: expected valid syntax, got '()" (lambda () (parse-fundef '())))
     
;;parse tests
(check-equal? (parse '(double 5)) (appC 'double (NumC 5)))
(check-exn #rx"VEBG3-parse: expected valid syntax, got '\\(1 a\\)" (lambda () (parse (list 1 'a))))