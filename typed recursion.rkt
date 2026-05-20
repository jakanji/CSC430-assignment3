#lang typed/racket

;; recursive program in untyped racket
#;(let ([fact (lambda (self n)
              (if (<= n 0)
                  1
                  (* n *self self (- n 1))))])
  (fact fact 6))

;;in typed rachet, the type of the first parameter
;;of fact is a function (fact). and the type of THAT
;;function's first argument is another function and so on...
#;(let ([fact : (-> (-> (-> ... N N) N N) N N)
               (lambda (self n)
              (if (<= n 0)
                  1
                  (* n *self self (- n 1))))])
  (fact fact 6))

;;to make this work we'd need to make a type for a recursive
;;function
(define-type FactType (-> FactType Narual Natural))
(let ([fact : FactType
       (lambda (self n)
              (if (<= n 0)
                  1
                  (* n *self self (- n 1))))])
  (fact fact 6))