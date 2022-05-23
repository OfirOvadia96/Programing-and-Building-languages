#lang pl 02

#|
Question 1 - BNF:
The LE grammer
Sections: a , b , c

  <LE> ::=   <num> (i)
           | <null> (ii)
           | <LETTER> (iii)
           | { list <LE>... } (iv)
           | { append <LsE> <LsE> ... } (v)
           | { cons <LE> <LsE> } (vi)
           | { ' <LETTER> ... } (vii)
 <LETTER>::= { A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
               a b c d e f g h i j k l m n o p q r s t u v w x y z} (viii)
 <LsE> ::= {list <LE>... } (ix)


sections 1.d:

(append (list 20854 1151) (cons 'ofir null)):
  
(cons 208 (cons (append (cons 'ofir null)(list 'ovadia 25)) null))
  
(null)
  (iii)

(cons 'ofir ('ovadia 'granit 58))
 

|#

;; need to be valid expressions:

#|
null
12
'boo
(cons 1 (cons 'two null))
(list 1 2 3)
(list (list (list (list 1 2 3))))
(append (list 1 2) (list 3 4) (list 5 6))
(list)
(append)
(cons 1 (cons (append (cons 'x null) (list 'y 'z)) null)) 1 (cons (append (cons 'x null) (list 'y 'z)) null)))
|#

;;need to be invalid expressions:

#|
(cons 1 2)
(list (3 4))
(quote boo)
(append 1 (list 2 3) 4)
(cons 1 2 null)
(cons 1 (2))
(cons 1 '())
'(1 2 3)
(cons '1 null)
(list ''a)
(car (list 1 2))
|#





;;Question 2:

;; 2.1: Make the language AE use infix syntax
;; 2.2: Allowing division by zero : return 999.

#|
The AE grammer (infix)

  <AE> ::= <num>
           | { <AE> + <AE> }
           | { <AE> - <AE> }
           | { <AE> * <AE> }
           | { <AE> / <AE> }
|#

(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])


(: parse-sexpr : Sexpr -> AE)
(define (parse-sexpr sxp)
  (match sxp
    [(number: n) (Num n)]
    [(list l '+ r) (Add (parse-sexpr l)(parse-sexpr r))]
    [(list l '- r) (Sub (parse-sexpr l)(parse-sexpr r))]
    [(list l '* r) (Mul (parse-sexpr l)(parse-sexpr r))]
    [(list l '/ r) (Div (parse-sexpr l)(parse-sexpr r))]
    [else (error 'parse-sexpr "bad syntax in ~s" sxp)]))


(: parse : String -> AE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))


;assuming we chose prefix form grammer with curly parentheses
(test (parse "3") => (Num 3))
(test (parse "{ 3  +  4 }") => (Add (Num 3) (Num 4)))
(test (parse "{ 3  -  4 }") => (Sub (Num 3) (Num 4)))
(test (parse "{ {3 - 2} + 4 }") => (Add (Sub (Num 3)(Num 2))(Num 4)))
(test ( parse "{10 * 10}") => (Mul (Num 10) (Num 10)))
(test ( parse "{10 / 10}") => (Div (Num 10) (Num 10)))
(test (parse "{1 + 2 + 3 + 4}") =error> "bad syntax")
(test (parse "{1 * 2 - 3 * 4}") =error> "bad syntax")
(test (parse "{1 / 2 * 3 * 4}") =error> "bad syntax")
(test (parse "{/ 1 4}") =error> "bad syntax")
(test (parse "{* 4 5}") =error> "bad syntax")
(test (parse "{+ 3 6}") =error> "bad syntax")
(test (parse "{- 2 7}") =error> "bad syntax")



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



#|
;;; ====== EVAL  ============== (allowing zero devision)s
; <AE> ::= <num>               a 
;          | { + <AE> <AE> }   b
;          | { - <AE> <AE> }   c

eval(<num>) = <num>
eval({+ E1 E2}) =  eval(E1) + eval(E2)
eval({- E1 E2}) =  eval(E1) - eval(E2)
|#



(: eval : AE -> Number)
(define (eval exp)
  (cases exp
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (cond
                 ;;if diviosion by zero return 999
                 [(eq? (eval r) 0) 999]
                 [else (/ (eval l) (eval r))])]))
      

(: run : String -> Number)
(define (run code)
  (eval (parse code)))


;;tests:
(test (eval (Num 3)) => 3)
(test (eval (Add (Num 3) (Num 4))) => 7)
(test (eval (Add (Sub (Num 3) (Num 2)) (Num 4))) => 5)
(test (eval (parse "{ 3 + 4 }")) => 7)
(test (eval (parse "3")) => 3)
(test (eval (parse "{ {3 - 2} + 4 }")) => 5)
(test (eval (parse "{1 + 2 + 3 + 4}")) =error> "bad syntax")
(test (eval (parse "{ 3 * {5 / 3} }")) => 5)

(test (run "{ 100 / 0}") => 999)
(test (run "{ 0 / 0}") => 999)
(test (run "{ 1 / 0}") => 999)
;;(test (run "{{100 / 0} + 3}") => 999) - ******need to check this case ******
(test (run "{ { 100 - 8 } / 0}") => 999)
(test (run "3") => 3)
(test (run "{ 3  +  4 }") => 7)
(test (run "{ 3  -  4 }") => -1)
(test (run "{ {3 - 2} + 4 }") => 5)
(test (run "{10 * 10}") => 100)
(test (run "{10 / 10}") => 1)
(test (run "{1 + 2 + 3 + 4}") =error> "bad syntax")
(test (run "{1 * 2 - 3 * 4}") =error> "bad syntax")
(test (run "{1 / 2 * 3 * 4}") =error> "bad syntax")
(test (run "{/ 1 4}") =error> "bad syntax")
(test (run "{* 4 5}") =error> "bad syntax")
(test (run "{+ 3 6}") =error> "bad syntax")
(test (run "{- 2 7}") =error> "bad syntax")
(test (run "{+ 1 2 3 4}") =error> "bad syntax")


;;Question 3:

( : sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lst)
   
  (foldl + 0 (map square lst))
  )


( : square : Number -> Number)
(define (square num)

   (* num num)
  )

;;tests:
(test (square 5) => 25)
(test (sum-of-squares '())=> 0)
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(2 3 4)) => 29)
(test (sum-of-squares '(0 0)) => 0)
(test (sum-of-squares '(10 0 0 0 0)) => 100)
(test (sum-of-squares '(-1 -2 -3)) => 14)
(test (sum-of-squares '(-1 2 -3)) => 14)
(test (sum-of-squares '(-1 -2 3)) => 14)

;;Question 4:
;;4.a:

(define-type BINTREE
  [Leaf Number]
  [Node BINTREE BINTREE])

;;tests:
(test (Leaf 1) => (Leaf 1))
(test  (Node (Leaf 1) (Leaf 2)) =>  (Node (Leaf 1) (Leaf 2)))
(test  (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) => (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))

;;4.b + 4.c:

( : tree-map : (Number -> BINTREE) BINTREE -> BINTREE)
(define (tree-map func bin-tree)
  
     (cases bin-tree
       [(Leaf num) (func num)]
       [(Node left right) (Node (tree-map func left) (tree-map func right))])
   )


;;for tests
( : add1 : Number -> BINTREE)
  (define (add1 num)
    
       (Leaf (+ num 1))
     )

( : Reduction1 : Number -> BINTREE)
  (define (Reduction1 num)
    
       (Leaf (- num 1))
     )

;;tests:
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))=> (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map add1 (Node (Leaf 10) (Node (Leaf 11) (Leaf 12))))=> (Node (Leaf 11) (Node (Leaf 12) (Leaf 13))))
(test (tree-map add1 (Node (Leaf 17) (Node (Leaf 18) (Leaf 19))))=> (Node (Leaf 18) (Node (Leaf 19) (Leaf 20))))
(test (tree-map Reduction1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))=> (Node (Leaf 0) (Node (Leaf 1) (Leaf 2))))
(test (tree-map Reduction1 (Node (Leaf 10) (Node (Leaf 11) (Leaf 12))))=> (Node (Leaf 9) (Node (Leaf 10) (Leaf 11))))
(test (tree-map Reduction1 (Node (Leaf 17) (Node (Leaf 18) (Leaf 19))))=> (Node (Leaf 16) (Node (Leaf 17) (Leaf 18))))
