#lang typed/racket
(require typed/rackunit)


(define-type ExprC (U NumC BinOp idC-def app-def))
(struct NumC ([n : Real]) #:transparent)
(struct BinOp ([op : (U '+ '* '- '/)] [frst : ExprC] [snd : ExprC]) #:transparent)
(struct idC-def ([s : Symbol]) #:transparent)
(struct app-def ([fun : Symbol] [arg : ExprC]))

(struct FundefC ([fundef : Symbol] [name : Symbol] [arg : Symbol] [body : ExprC]) #:transparent)

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

;; interpretation evaluation for VEBG language
(define (interp [a : ExprC]) : Real
  (match a
    [(NumC n) n]
    #;[(idC-def i) ...]
    #;[(app-def a e) ...]
    [(BinOp o l r) (cond
                     [(equal? o '/) ((BinopTableDiv r) (interp l ) (interp r ))]
                     [else ((BinopTable o) (interp l ) (interp r ))])]))

;;parse the given concret syntax into an AST
(define (parse [prog : Sexp]): ExprC
  (match prog
    [(? real? n) (NumC n)]
    [(list '+ left right) (BinOp '+ (parse left) (parse right))]
    [(list '* left right) (BinOp '* (parse left) (parse right))]
    [(list '/ left right) (BinOp '/ (parse left) (parse right))]
    [(list '- left right) (BinOp '- (parse left) (parse right))]
    [(? symbol? a) (idC-def a)]
    [other (error 'VEBG3-parse "expected valid syntax, got ~e" other)]))

;;parser for function definitions
;;takes an s expresison and returns a FundefC
(define (parse-fundef [func : Sexp]): FundefC
  (match func
    [(list 'named-fn (? symbol? name) (? symbol? arg) body) (FundefC 'named-fn name arg (parse body))]
    [other (error 'VEBG3-parse-fundef "expected valid syntax, got ~e" other)]))

;;helper function for parse-fundef
;;takes a symbol and list of FundefCs and returns a FundefC

;;takes an s-expression and calles parser and interp
(define (top-interp [expr : Sexp]) : Real (interp (parse expr)))

;;---tests------------------------------------------------------------------------------------------------

;;top-interp tests
(check-equal? (top-interp '(* (+ 1 (- 10 (/ 1 1))) 2))20)
(check-exn #rx"VEBG3-BinopTableDiv: cannot divide by zero" (lambda () (top-interp '(/ 1 0))))

;;function parse tests
(check-equal? (parse-fundef '(named-fn double x (+ x x)))
              (FundefC 'named-fn 'double 'x (BinOp '+ (idC-def 'x) (idC-def 'x))))
(check-exn #rx"VEBG3-parse-fundef: expected valid syntax, got '()" (lambda () (parse-fundef '())))
     
;;parse tests
(check-exn #rx"VEBG3-parse: expected valid syntax, got '\\(1 a\\)" (lambda () (parse (list 1 'a))))