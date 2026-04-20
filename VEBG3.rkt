#lang typed/racket
(require typed/rackunit)

(define-type ExprC (U NumC PlusC MultC))
(struct NumC ([n : Real]) #:transparent)
(struct PlusC ([left : ExprC] [right : ExprC]) #:transparent)
(struct MultC ([left : ExprC] [right : ExprC]) #:transparent)

;; interpretation evaluation for VEBG language
(define (interp [a : ExprC]) : Real
  (match a
    [(NumC n) n]
    [(PlusC l r) (+ (interp l) (interp r))]
    [(MultC l r) (* (interp l) (interp r))]))

;;parse the given concret syntax into an AST
(define (parse [prog : Sexp]): ExprC
  (match prog
    [(? real? n) (NumC n)]
    [(list '+ left right) (PlusC (parse left) (parse right))]
    [(list '* left right) (MultC (parse left) (parse right))]
    [other (error 'parse "Arith expected valid syntax, got ~e" other)]))

