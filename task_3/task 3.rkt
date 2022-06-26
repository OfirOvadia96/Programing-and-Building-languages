#lang pl

;; Question 1: change AE language to postfix , add two operators: power && sqr.

#|

The AE grammer postfix

  <AE> ::= <num>
           | { <AE> <AE> + }
           | { <AE> <AE> - }
           | { <AE> <AE> * }
           | { <AE> <AE> / }
           | { <AE> <AE> power }
           | { <AE> sqr }
|#

;;==== type =======
(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE]
  [Power AE AE] ;; new var
  [Sqr AE]) ;; new var

;; ======= parse =========
(: parse-sexpr : Sexpr -> AE)
(define (parse-sexpr sxp)
  (match sxp
    [(number: n) (Num n)]
    [(list l r '+) (Add (parse-sexpr l)(parse-sexpr r))]
    [(list l r '-) (Sub (parse-sexpr l)(parse-sexpr r))]
    [(list l r '*) (Mul (parse-sexpr l)(parse-sexpr r))]
    [(list l r '/) (Div (parse-sexpr l)(parse-sexpr r))]
    [(list l r 'power) (Power (parse-sexpr l) (parse-sexpr r))] ;; pase to power:  l = base , r = p 
    [(list arg 'sqr) (Sqr (parse-sexpr arg))] ;; pase to sqr : arg = base for sqr
    [else (error 'parse-sexpr "bad syntax in ~s" sxp)]))


(: parse : String -> AE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))

#|
The goal of parse:
Input:  string describing the program
Output: Abstract Syntax Tree (or an exception if the string is not a valid program)

Two main phases:
1. Read -- turn the string into a simple data structure (we will use the Racket type Sexpr).
2. Actual Parsing -- turn an Sexpr into an AST


Definition of the pl type Sexpr:
Basis -- any Number/Symbol is an Sexpr
General -- any list of Sexpr is an Sexpr

|#


;;; ====== EVAL  ==============

(: eval : AE -> Number)
(define (eval exp)
  (cases exp
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]
    [(Sqr arg) (* (eval arg) (eval arg))]
    [(Power l r) (if (eq? (isInteger (eval r))#t)  (MyPower (eval l) (eval r))  (error 'eval "power expects an integer power,got:") )]))

(: isInteger : Number ->(U #f #t))
 (define (isInteger exp)
   (if (integer? exp) #t #f) )

(: MyPower : Number Number -> Number)
(define (MyPower base p)
  (cond
    [(= p 0) 1]
    [(= p 1) base]
    [(< p 0) (MyPower (/ 1 base) (* -1 p))]
    [else (* base (MyPower base (- p 1)))]))

(: run : String -> Number)
(define (run code)
  (eval (parse code)))


 ;; ========== Tests - (Question 1): =============
;;Auxiliary functions
(test (isInteger 5) => #t)
(test (isInteger 5.1) => #f)
(test (MyPower 2 5) => 32)
(test (MyPower 2 4) => 16)
(test (MyPower 1/2 4) => 1/16)
(test (MyPower 4 -2) => 1/16 )

(test (run "{1/2 0 power}") => 1)
(test (run "{1/2 3 power}") => 1/8)
(test (run "{10 0 power}") => 1)
(test (run "3") => 3)
(test (run "{3 4 +}") => 7)
(test (run "{{3 4 -} 7 +}") => 6)
(test (run "{{3 4 power} 7 +}") => 88)
(test (run "{{2 4 power} {5 sqr} +}") => 41)
(test (run "{{2 4/5 power} {5 sqr} +}") =error> "eval: power expects an integer power,got:")
(test (parse "{3  4 + }") => (Add (Num 3) (Num 4)))
(test (parse "3") => (Num 3))
(test (parse "{{3 2 - }  4 +}") => (Add (Sub (Num 3) (Num 2)) (Num 4)))
(test (parse "{1 2 3 4 +}") =error> "bad syntax")
(test (run "3") => 3)
(test (run "{96 {3 {4 sqr} *} /}") => 2)
(test (run "{96 {{10 -2 power} {3 {4 sqr} *} +} /}") => 9600/4801)
(test (run "{ + {- 3 2}  4 }") =error>"bad syntax")
(test (run "{+ 1 2 3 4}") =error> "bad syntax")


;; Question 2:

;; ========== types ==========
;; LE abstract syntax trees
(define-type LE = (U LIST ATOM))
 
;; LIST abstract syntax trees
(define-type LIST
  [Cons LE LIST]
  [Append (Listof LIST)]
  [MyList (Listof LE)]
  [LENull]
  )

;; ATOM abstract syntax trees
(define-type ATOM
  [LENum Number]
  [LEId Symbol]

  )
;; ========== parse =============
(: parse-sexpr->LEs : (Listof Sexpr) -> (Listof LE))
 ;; converts a list of s-expressions into a list of LEs
 (define (parse-sexpr->LEs sexprs)
 (map parse-sexprLE sexprs))

;; converts a list of s-exprs into a list of LISTs
(: parse-sexpr->LISTs : (Listof Sexpr) -> (Listof LIST))
(define (parse-sexpr->LISTs sexprs)
 (map parse-sexpr->LIST sexprs))

(: parse-sexpr->LIST : Sexpr -> LIST)
 (define (parse-sexpr->LIST sexpr)
 (let ([ast (parse-sexprLE sexpr)])
 (if (LIST?
ast)
ast
 (error 'parsesexprLE "expected LIST; got~s" ast))))

;; to convert s-expressions into LEs
(: parse-sexprLE : Sexpr -> LE)
(define (parse-sexprLE sexpr)
 (match sexpr
 [(number: n) (LENum n)]
 ['null  (LENull)]
 [(symbol: s) (LEId s)]
 [(cons 'list arg) (MyList (parse-sexpr->LEs arg))]
 [(cons 'append lst) (Append (parse-sexpr->LISTs lst))]
 [(list 'cons lhs rhs) (Cons (parse-sexprLE lhs) (parse-sexpr->LIST rhs))]
 [else (error 'parsesexprLE "bad syntax in ~s" sexpr)]))



 ;; parses a string containing a LE expression to a
 ;; LE AST
(: parseLE : String -> LE)
(define (parseLE str)
 (parse-sexprLE (string->sexpr str)))


;; ========== Eval ============
#| Formal specs for `eval':
  eval(N) = N ;; for numbers
  eval(Sym) = 'Sym ;; for symbols
  eval({list E ...}) = (list eval(E) ...)
  eval({cons E1 E2}) = if eval(E2) = (list E), then
  (cons eval(E1) eval(E2))
  else error eval({append E ...}) =
  if eval(E) = (list E') for all expressions E, then
  (append eval(E) ...)
|#

;; evaluates LE expressions by reducing them to lists
(: eval-append-args : (Listof LE) -> (Listof (Listof Any)))
(define (eval-append-args exprs)
(if (null? exprs)
   null
(let ([fst-val (evalLE (first exprs))])
(if (list? fst-val)
(cons fst-val (eval-append-args (rest exprs)))
(error 'evalLE "append argument: expected List got ~s" fst-val)))))

;; evaluates LE expressions by reducing them to numbers
(: evalLE : LE -> Any)
(define (evalLE expr)(if (LIST? expr)
(cases expr
  [(MyList lst) (map evalLE lst)]
  [(Cons l r) (let ([fst-val (evalLE r)])
                (if (list? fst-val)
                    (cons (evalLE l) fst-val)
                  (error 'evalLE "cons argument: expected List got:~s" fst-val)))]
  [(Append lst) (apply append (eval-append-args lst))]
  [(LENull) null])
(cases expr
  [(LENum n) n]
  [(LEId id) id]))
  )


(: runLE : String -> Any)
(define (runLE str)
(evalLE (parseLE str)))

;; ========== Tests - (Question 2): =============
(test (parseLE "6")=> (LENum 6))
(test (runLE "null") => null)
(test (runLE "12") => 12)
(test (runLE "boo") => 'boo)
(test (runLE "{cons 1 {cons two null}}") => '(1 two))
(test (runLE "{list 1 2 3}") => '(1 2 3))
(test (runLE "{list {cons}}") =error> "parsesexprLE: bad syntax in (cons)")
(test (runLE "{list {cons 2 1}}") =error>"parsesexprLE: expected LIST; got")
(test (runLE "{append {list 1 2 3} {list 4 5 6}}") => '(1 2 3 4 5 6))
;;(test (runLE "{cons 5 6}") =error> " parsesexprLE: expected LIST; got")
;;(test (runLE "{append 10 20}") =error> "bad syntax")





