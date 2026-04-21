#lang typed/racket
(require typed/rackunit)

(define-type ExprC (U NumC BinOp))
(struct BinOp ([op : (U '+ '*)] [frst : ExprC] [snd : ExprC])
  #:transparent)
(struct NumC ([n : Real]) #:transparent)

;; interpretation evaluation for VEBG language
(define (interp [a : ExprC]) : Real
  (match a
    [(NumC n) n]
    [(BinOp o l r) (cond
                     [(equal? o '+) (+ (interp l) (interp r))]
                     [(equal? o '*) (* (interp l) (interp r))])]))

;;parse the given concret syntax into an AST
(define (parse [prog : Sexp]): ExprC
  (match prog
    [(? real? n) (NumC n)]
    [(list '+ left right) (BinOp '+ (parse left) (parse right))]
    [(list '* left right) (BinOp '* (parse left) (parse right))]
    [other (error 'parse "expected valid syntax, got ~e" other)]))

;;takes an s-expression and calles parser and interp
(define (top-interp [expr : Sexp]) : Real (interp (parse expr)))


;;top-interp tests
(check-equal? (top-interp '(* (+ 1 1) 2)) 4)
;;parse tests
(check-exn #rx"parse: expected valid syntax, got 'a" (lambda () (parse 'a)))
