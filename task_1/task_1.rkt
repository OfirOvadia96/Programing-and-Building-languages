#lang pl

#|
  Task 1
  Author : Ofir Ovadia
|#

#|
  Question 1 - min&max
  description:
   return from 5 numbers List of min and max number
  Auxiliary functions:
  min - return the minimum number of the given numbers
  max - return the maximum number of the given numbers
|#


;;min function - return the minimum number of the given numbers

(: my_min : Number Number Number Number Number -> Number)
  (define (my_min n1 n2 n3 n4 n5)
    (
     ;;Comparing each of the five numbers with all of them for the purpose of finding the minimum number
     cond [(and (<= n1 n2) (<= n1 n3) (<= n1 n4) (<= n1 n5)) n1]
          [(and (<= n2 n1) (<= n2 n3) (<= n2 n4) (<= n2 n5)) n2]
          [(and (<= n3 n1) (<= n3 n2) (<= n3 n4) (<= n3 n5)) n3]
          [(and (<= n4 n1) (<= n4 n2) (<= n4 n3) (<= n4 n5)) n4]
          [else n5]
     ))

;; max function - return the maximum number of the given numbers

(: my_max : Number Number Number Number Number -> Number)
  (define (my_max n1 n2 n3 n4 n5)
    (
     ;;Comparing each of the five numbers with all of them for the purpose of finding the maximum number
     cond [(and (>= n1 n2) (>= n1 n3) (>= n1 n4) (>= n1 n5)) n1]
          [(and (>= n2 n1) (>= n2 n3) (>= n2 n4) (>= n2 n5)) n2]
          [(and (>= n3 n1) (>= n3 n2) (>= n3 n4) (>= n3 n5)) n3]
          [(and (>= n4 n1) (>= n4 n2) (>= n4 n3) (>= n4 n5)) n4]
          [else n5]
     ))
    

(: min&max : Number Number Number Number Number -> (Listof Number))
   (define (min&max  n1 n2 n3 n4 n5)
  (
    list (my_min n1 n2 n3 n4 n5) (my_max n1 n2 n3 n4 n5) ;;Send to 2 simple auxiliary functions and return as a list as requested
     ))

#|
   Question 2.a - sublist-numbers (tail recursive)
   description:
     A function that receives a list of Any and returns a list of numbers that were within the input list
     This function calls an auxiliary function to perform tail recursion
   Auxiliary function:
    tail-helper - return the result: List of numbers
|#

(: sublist-numbers : (Listof Any) -> (Listof Number))
   (define (sublist-numbers lst)
     (
      tail-helper lst '() ;;Send to auxiliary function
      ))

(: tail-helper : (Listof Any) (Listof Number) -> (Listof Number))
   (define (tail-helper lstAny lstNum)
     (
       cond
         ;;Stopping conditions / handling an empty list :Returns the desired number list
         [(null? lstAny) lstNum]
         ;;If the member is a number, we will add it to the list of numbers and continue recursively to go through the input list
         [ (number? (first lstAny)) (tail-helper (rest lstAny) (cons (first lstAny) lstNum))]
         ;;If the organ does not count, we will recursively go over the input list
         [else (tail-helper (rest lstAny) lstNum )]
       ))


#|
   Question 2.b - min&max-lists
   Auxiliary function:
    apply - Going through list
    apply min - give the min number from list
    apply max - give the max number from list
    sublist-numbers - (from question 2.a) return listof Number from Listof Any
|#


(: min&max-lists : (Listof (Listof Any)) -> (Listof (Listof Any)))
   (define (min&max-lists lst)
     (
      cond
        [(null? lst) lst]
        ;;If there are no numbers in the current internal list, we will become null and continue to the next internal list
        [(null? (sublist-numbers (first lst))) (cons null(min&max-lists (rest lst)))]
        ;;If there are numbers in the current internal list, we will become a list of min and max and continue to the next internal list
        [else (cons (list (apply min (sublist-numbers (first lst))) (apply max (sublist-numbers (first lst)))) (min&max-lists(rest lst)))]
      ))


#|
 Question 3.a - KeyStack empty constructor

|#

(define-type KeyStack
  [EmptyKS]
  [push Symbol String KeyStack]
  )
#|
 Question 3.b - Push
|#

( : Push : Symbol String KeyStack -> KeyStack)
  (define (Push key str stck)
   (
    push key str stck
    ))

#|
 Question 3.c - search-stack
|#
( : search-stack : Symbol KeyStack -> (U String #f))
  (define (search-stack key stck )
    (
     ;;Check the appropriate constructor
     cases stck
        [(EmptyKS) #f]
       [(push k str stck)
        (cond
            ;;If the keys match - we return the first value of the same key
            [(eq? key k) str]
            ;;If the keys do not match we will continue to check the next organ in the keystack
            [else (search-stack key stck)])]
     ))

#|
 Question 3.d - pop-stack
|#
( : pop-stack : KeyStack -> (U KeyStack #f))
  (define (pop-stack stck)
    (
     cases stck
      ;;If empty we return f
        [(EmptyKS) #f]
        ;;If not empty, we will return without the first value
        [(push k str stck) stck]
     ))

  
#|
  Tests:
|#

;;Question 1

(test (min&max 2 3 2 7 5) => '(2 7))
(test (min&max 2 1000 2 7 5) => '(2 1000))
(test (min&max 2 3 30 7 5) => '(2 30))
(test (min&max 2 -5 2 7 5) => '(-5 7))
(test (min&max 2 3 -10 7 5) => '(-10 7))
(test (min&max 2 3 2 7 -100) => '(-100 7))
(test (min&max 8 8 8 8 8) => '(8 8))
(test (min&max 8 8 8 -1 8) => '(-1 8))
(test (min&max 8 8 8 -1 9.5) => '(-1 9.5))
(test (min&max 300 8 8 -100 9.5) => '(-100 300))

;;Question 2.a:

(test (sublist-numbers (list 'any "Benny" 10 'OP 8)) => '(8 10))
(test (sublist-numbers '(any "Benny" OP (2 3))) => null)
;(test (sublist-numbers (list 300 "ofir" '30 'OP 9)) => '(9 300)) - need to check if '30 called number or symbol
(test (sublist-numbers '(any "Benny" OP ('a 'b) '())) => null)
(test (sublist-numbers (list 'any "Benny" 10 'OP 8)) => '(8 10))
(test (sublist-numbers '()) => null)
(test (sublist-numbers '(1 2 3 4 5 6 7 8 9 10)) => '(10 9 8 7 6 5 4 3 2 1))

;;Question 2.b:

(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3)))) => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ())) => '((1 5) (1 7) ()))
(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP 2 3))) => '((8 10) (2 3)))
(test (min&max-lists '((2 2 1 2 L) (4 5 6 7 3 2 1) ())) => '((1 2) (1 7) ()))
(test (min&max-lists '(())) => '(()))

;;Question 3.a:

(test (EmptyKS) => (EmptyKS))

;;Question 3.b:
(test (Push 'a "A" (EmptyKS)) => (Push 'a "A" (EmptyKS)))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))

;;Question 3.c:

(test (search-stack 'a (EmptyKS)) => #f)
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (search-stack 'b (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "B")

;;Question 3.d: 

(test (pop-stack (EmptyKS)) => #f)
(test (pop-stack (Push 'c "C" (EmptyKS))) => (EmptyKS))
(test (pop-stack (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "A" (EmptyKS)))
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'c "C" (EmptyKS))))) => (Push 'b "B" (Push 'c "C" (EmptyKS))))

