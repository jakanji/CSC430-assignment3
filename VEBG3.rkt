#lang typed/racket
(require typed/rackunit)

(define-type ExprC (U NumC BinOp idC appC Ifleq0C))
(struct NumC ([n : Real]) #:transparent)
(struct BinOp ([op : (U '+ '* '- '/)] [frst : ExprC] [snd : ExprC]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct appC ([fun : Symbol] [arg : (Listof ExprC)]) #:transparent)
(struct Ifleq0C ([test : ExprC] [thn : ExprC] [els : ExprC]) #:transparent)

(struct FundefC ([name : Symbol] [arg : (Listof Symbol)] [body : ExprC]) #:transparent)

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
    [(appC f (list a)) (appC f (list (subst what for a)))]
    [(appC f (list a ...)) (error "unimplemented multi-arg app")]
    [(BinOp s l r) (cond
                     [(equal? s '/) (BinOp '/ (subst what for l) (subst what for r))]
                     [(equal? s '+) (BinOp '+ (subst what for l) (subst what for r))]
                     [(equal? s '*) (BinOp '* (subst what for l) (subst what for r))]
                     [(equal? s '-) (BinOp '- (subst what for l) (subst what for r))])]
    [(Ifleq0C tst thn els)
     (Ifleq0C (subst what for tst)
              (subst what for thn)
              (subst what for els))]))

;; interpretation evaluation for VEBG language
(define (interp [a : ExprC] [fds : (Listof FundefC)]) : Real
  (match a
    [(NumC n) n]
    [(idC i) (error 'VEBG3-interp "unbound identifier error: ~e" i)]
    [(appC fun (list arg)) (define fd (get-fundef fun fds))
                    (interp (subst arg
                                   (first (FundefC-arg fd))
                                   (FundefC-body fd)) fds)]
    [(BinOp o l r) (cond
                     [(equal? o '/) ((BinopTableDiv r) (interp l fds) (interp r fds))]
                     [else ((BinopTable o) (interp l fds) (interp r fds))])]
    [(Ifleq0C tst thn els)
     (if (<= (interp tst fds) 0)
         (interp thn fds)
         (interp els fds))]))
                              
                        
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
    [(list (? symbol? fun) arg) (appC fun (list (parse arg)))] 
    [(? symbol? a) (idC a)]
    [other (error 'VEBG3-parse "expected valid syntax, got ~e" other)]))

;;parser for function definitions
;;takes an s expresison and returns a FundefC
(define (parse-fundef [func : Sexp]): FundefC
  (match func
    [(list (? symbol? name) (? symbol? arg) body) (FundefC name (list arg) (parse body))]
    [other (error 'VEBG3-parse-fundef "expected valid syntax, got ~e" other)]))

;;takes an s expression and returns a list of FundefCs

;;takes an s-expression and calles parser and interp
(define (top-interp [expr : Sexp]) : Real
  (interp
   (parse expr)
   (list (FundefC 'a (list 'b) (idC 'c)))))

;;---tests------------------------------------------------------------------------------------------------

;;get-fundef tests
(check-equal? (get-fundef 'target (list
                                   (FundefC 'a '(e) (idC 'f))
                                   (FundefC 'target '(arg) (NumC 2))))
              (FundefC 'target '(arg) (NumC 2)))
(check-exn #rx"VEBG-get-fundef: reference to undefined function" (lambda () (get-fundef 'a '())))

;;interp tests
(check-equal? (interp (appC 'add (list (NumC 5))) (list (FundefC 'add '(x) (BinOp '+ (idC 'x) (appC 'sub (list (NumC 1)))))
                                                 (FundefC 'sub '(z) (BinOp '- (idC 'z) (appC 'mult (list (NumC 2)))))
                                                 (FundefC 'mult '(a) (BinOp '* (idC 'a) (appC 'div (list (NumC 1)))))
                                                 (FundefC 'div '(y) (BinOp '/ (idC 'y) (NumC 1)))))
              4)
(check-exn #rx"VEBG3-interp: unbound identifier error: 'y"
           (lambda () (interp (appC 'div (list (NumC 5)))
                              (list (FundefC 'div (list 'x) (BinOp '/ (idC 'y) (NumC 1)))))))

;;ifleq tests
(check-equal?
 (interp (Ifleq0C (NumC 0) (NumC 1) (NumC 2)) '())
 1)
(check-equal?
 (interp (Ifleq0C (NumC -4) (NumC 1) (NumC 2)) '())
 1)
(check-equal?
 (interp (Ifleq0C (NumC 5) (NumC 1) (NumC 2)) '())
 2)
                                                               
;;top-interp tests
(check-equal? (top-interp '(* (+ 1 (- 10 (/ 1 1))) 2))20)
(check-exn #rx"VEBG3-BinopTableDiv: cannot divide by zero" (lambda () (top-interp '(/ 1 0))))

;;function parse tests
(check-equal? (parse-fundef '(double x (+ x x)))
              (FundefC 'double (list 'x) (BinOp '+ (idC 'x) (idC 'x))))
(check-exn #rx"VEBG3-parse-fundef: expected valid syntax, got '()" (lambda () (parse-fundef '())))
(check-equal?
 (interp (appC 'dec-if-pos (list (NumC 5)))
         (list (FundefC 'dec-if-pos '(x)
                        (Ifleq0C (idC 'x)
                                 (idC 'x)
                                 (BinOp '- (idC 'x) (NumC 1))))))
 4)

(check-equal?
 (interp (appC 'dec-if-pos (list (NumC -2)))
         (list (FundefC 'dec-if-pos  '(x)
                        (Ifleq0C (idC 'x)
                                 (idC 'x)
                                 (BinOp '- (idC 'x) (NumC 1))))))
 -2)

(check-exn #rx"VEBG3-parse-fundef: expected valid syntax, got '()" (lambda () (parse-fundef '())))

     
;;parse tests
(check-equal? (parse '(double 5)) (appC 'double (list (NumC 5))))
(check-exn #rx"VEBG3-parse: expected valid syntax, got '\\(1 a\\)" (lambda () (parse (list 1 'a))))

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
 (interp (parse '(ifleq0? 0 44 99)) '())
 44)

(check-equal?
 (interp (parse '(ifleq0? 3 44 99)) '())
 99)

(check-equal?
 (interp (parse '(ifleq0? (- 2 5) (+ 1 1) (+ 10 10))) '())
 2)