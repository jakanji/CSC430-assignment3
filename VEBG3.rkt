#lang typed/racket
(require typed/rackunit)

;;make hash table for binary operations
(define BinOpTable (make-hash))
(hash-set! BinOpTable '+ +)
(hash-set! BinOpTable '* *)

;;
(define-type ExprC (U NumC BinOp idC-def app-def))
(struct NumC ([n : Real]) #:transparent)
(struct BinOp ([op : (U '+ '*)] [frst : ExprC] [snd : ExprC]) #:transparent)
(struct idC-def ([s : symbol]) #:transparent)
(struct app-def ([fun : symbol] [arg : ExprC]))

(define-type FundefC  ([name : symbol] [arg : symbol] [body : ExprC]))

;; interpretation evaluation for VEBG language
(define (interp [a : ExprC]) : Real
  (match a
    [(NumC n) n]
    [(BinOp o l r) (define op (hash-ref BinOpTable o));;TODO: need to figure out how to apply op correctly
                   (cond
                     [(equal? o '+) (+ (interp l) (interp r))]
                     [(equal? o '*) (* (interp l) (interp r))])]))

;;parse the given concret syntax into an AST
(define (parse [prog : Sexp]): ExprC
  (match prog
    [(? real? n) (NumC n)]
    [(list '+ left right) (BinOp '+ (parse left) (parse right))]
    [(list '* left right) (BinOp '* (parse left) (parse right))]
    [other (error 'parse "expected valid syntax, got ~e" other)]))

;;parser for function definitions
;;takes an s expresison and returns a FundefC
(define (parse-fundef [func : Sexp]): FundefC
  (match func
    [(list 'named-fn (? symbol? name) (? symbol? arg) body) (FundefC name arg (parse body))]
    [other (error 'parse-fundef "expected valid syntax, got ~e" other)]))

;;takes an s-expression and calles parser and interp
(define (top-interp [expr : Sexp]) : Real (interp (parse expr)))

;;---tests------------------------------------------------------------------------------------------------

;;top-interp tests
(check-equal? (top-interp '(* (+ 1 1) 2)) 4)
;;parse tests
(check-exn #rx"parse: expected valid syntax, got 'a" (lambda () (parse 'a)))
