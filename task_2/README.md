<h1> Task 2 </h1>

<h3>The task contains a single rkt file:</h3>

* task_2.rkt

<h3>consisting of:</h3>

* New BNF for a new language called LE (Similar to "List Expressions")

    valid functions that can be used in these expressions are: 
    1. cons 
    2. list 
    3. append
    4. null

* The AE language we learned in class with the following changes:
 
  1. Changing the language AE from prefix form to infix form
  2. Allow division by 0 - returns 999


* square function - returns the square value of the number it receives

* sum-of-squares - returns the sum of the square numbers of the list of numbers it receives 

* BINTREE type definition
   
   Its variants:
   
   1. leaf
   2. Node
   
   tree-map function - gets in a numeric function f and a binary tree, and returns a tree with the same shape but using f(n) for values in its leaves
   

*  Tests

<h3>Using tools of the PL language:</h3>

* map - gets a list and a proc and returns a list created by running the proc procedure on each of the elements of the list it receives

* foldl - consumes a combiner function, an initial value, and an input list           
          It returns a value that is created in the following way:
          
        1. for the empty list the initial value is returned
        2. for a list with one item it uses the combiner function with this item and the initial value
        3. for two items, it uses the combiner function with the first and the result of folding the rest (a one-item list), etc.
   
