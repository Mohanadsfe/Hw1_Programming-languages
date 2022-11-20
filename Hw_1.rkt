#lang pl

#|Question Number 1A
a recursive function open-list that consumes a list of lists
(where the type of the elements in the inner lists in a Number)
and returns a list contains all the elements of the inner lists concatenated in the same order.
|#
;; The idea of the result is to iteration over the list of lists ,recursive append list to list ,continue in the same way until the func return list null



(: open-list : (Listof(Listof Number))   -> (Listof Number))

(define (open-list listoflists)
  (cond [(null? listoflists) null]
        [else (append (first listoflists)  (open-list (rest listoflists)))]
        
        ))

(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90))
(test (open-list '((1 2 3) ())) => '(1 2 3))
(test (open-list '((6) (7 8) )) => '(6 7 8))
(test (open-list '((1) () (4) () (-1) (90))) => '(1 4 -1 90))
(test (open-list '(() () () ())) => '())





#|Question Number 1B
afunctionmin&maxthatconsumesalistoflists(wherethetype of the elements in the inner lists in a Number) and returns a list
containing the minimum and the maximum of the values in the inner lists.
|#

;; list-max function find accept listof number and return number,the maximum number in the list
;; recursive function, go over the list until get -inf.0 ,then beggining the check using function max ,and return with recursive way.

(: list-max : (Listof Number) -> Number)

(define (list-max mylist) (cond
[(null? mylist) -inf.0]
[else (max (first mylist) (list-max (rest mylist)))]))

(test (list-max '(1 5 3 0) ) => 5.0)
(test (list-max '(1 -5 5 10 30 3 0)) => 30.0)
(test (list-max '()) => -inf.0)
(list-max (list 1 2 3 4 15 5 6 -1 -7 11))


;; list-min function find accept listof number and return number,the minimum number in the list
;; recursive function, go over the list until get +inf.0 ,then beggining the check using function min ,and return with recursive way.

(: list-min : (Listof Number) -> Number)

(define (list-min mylist) (cond
[(null? mylist) +inf.0]
[else (min (first mylist) (list-min (rest mylist)))]))

(test (list-min '(1 5 3 0)) => 0.0)
(test (list-min '(1 -5 5 10 30 3 0)) => -5.0)
(test (list-min '()) => +inf.0)
(list-min (list 1 2 3 4 15 5 6 -1 -7 11))




;; This function it's accept list of lists of number, and return list with two numbers (the minimum, the maximum)

(: min&max : (Listof(Listof Number))  -> (Listof Number))

  
;; check ,if the list of lists null then return the default max and min (+/-inf.0)
;; if not, then call the two function (min and max) and extend them in one list, return it.
(define (min&max listoflists)
   
  
        ( list (list-min(open-list listoflists)) (list-max(open-list listoflists)))
        )
        
(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0))
(test (min&max '((1 1/2 3))) => '(0.5 3.0))
(test (min&max '((1 1/2))) => '(0.5 1.0))
(test (min&max '((1))) => '(1.0 1.0))
(test (min&max '((1) (3/4))) => '(0.75 1.0))
(test (min&max '(())) => '(+inf.0 -inf.0))
(test (min&max '(() () () () ())) => '(+inf.0 -inf.0))
(min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90)))
(test (min&max '((1))) => '(1.0 1.0))
(test (min&max '((1) (1 5))) => '(1.0 5.0))



#|Question 1C
To solve the “problem” in part “b” you may want to use the built in Racket apply,and min/max.
Write a function min&max_apply that does exactly what you did in part “b” but using apply function.|#


(: min&max-apply : (Listof(Listof Number))  -> (Listof Number))


(define (min&max-apply listoflists)
   (cond [(null? (open-list listoflists)) '(+inf.0 -inf.0)]
 
        [else( list (apply min (open-list listoflists)) (apply max (open-list listoflists)))] ;if the listoflists is not null ,then convert the listoflists to list (using the function open-list)
        ; then get the minimum value using apply min (that work on list not number), the same to the max , and then converte the two numbers to list. 
        ))
        
(test (min&max-apply '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1 233))
(test (min&max-apply '((1 1/2 3))) => '(1/2 3))
(test (min&max-apply '((1 1/2))) => '(1/2 1))
(test (min&max-apply '((1))) => '(1 1))
(test (min&max-apply '((1) (3/4))) => '(3/4 1))
(test (min&max-apply '(())) => '(+inf.0 -inf.0))
(test (min&max-apply '(() () () () ())) => '(+inf.0 -inf.0))
(min&max-apply '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90)))
(min&max-apply '(() () () () ())) 


#|Question 2
In this question we will implement a simple Table data structure.
In this data structure you will need to define a new type called Table.
 Each element in the table will be keyed (indexed) with a symbol.
 In the following the operations that you are required to implement are detailed below, together with some guidance.
|#


;; Q2.A
(define-type Table
  [EmptyTbl];; Implement the empty table EmptyTbl – this should be a variant of the data type (constructor).
  [add2-Table Symbol String Table]
  )

;;test
(test (EmptyTbl) => (EmptyTbl))
(test (Table? (EmptyTbl)) => #t)

;; Q2.B

; first i'm build a new constructor with paramters (key value table) (Symbol String Table)
; if the table is empty then build using using the constructor with paramters 
(: Add : Symbol String Table -> Table)

(define (Add key value table)
  (add2-Table key value table))

(test (Add 'b "B" (Add 'a "A" (EmptyTbl))) => (Add 'b "B" (Add 'a "A" (EmptyTbl))))

(test (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) => (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))


;;Q2.C

#|
function search-table
it accept the paramters: table , symbol key
the table it's that should serach on.
key - what is the symbol we search it on the table;
The base state, is when the list is empty then return false ,that meanning "not found the key inside the table)
if not empty, and we find the key then return string of the value


|#


(: search-table : Symbol Table -> (U String #f))

(define (search-table key table)
  (cases table
    [(EmptyTbl) #f] ;; if the table is empty then there no what to search, return false.
    [(add2-Table k v t) ;; else
     (cond [(equal? k key) v] ;if the keys are equal then return the value ,end.
           [else (search-table key t)])] ; else , call the fucntion with key and the table t ,that inside...table 

    ))


(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl))))) => #f)
(test (search-table 'a (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl))))) => "AAA")
(test (search-table 'b (Add 'a "AAA" (Add 'b "" (Add 'a "A" (EmptyTbl))))) => "")


;;Q2.D
;; function remove-item

(: remove-item : Table Symbol -> Table)
 
(define (remove-item t s)
  (cases t
    [(EmptyTbl) (EmptyTbl)] ;if the table is empty, then there no what to delete, just return a empty table
    [(add2-Table k v table) ;if not ,it's from this variant 
     (cond [(equal? s k) table] ;then check if the symbol we accept and with key in this table equal or not, if yes then return the table that for add not what we accept in the fucntion.
           [else (Add k v (remove-item table s))
                 ])]
    ))

(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) 'a) => (Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (remove-item (Add 'a "AAA" (Add 'a "" (Add 'a "A" (EmptyTbl)))) 'a) => (Add 'a "" (Add 'a "A" (EmptyTbl))))
(test (remove-item (Add 'a "AAA" (Add 'a "" (Add 'a "A" (EmptyTbl)))) 'b) => (Add 'a "AAA" (Add 'a "" (Add 'a "A" (EmptyTbl)))))
(test (remove-item (Add 'a "AAA" (Add 'b "" (Add 'c "C" (EmptyTbl)))) 'c) => (Add 'a "AAA" (Add 'b "" (EmptyTbl))))
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) 'b) => (Add 'a "AAA" (Add 'a "A" (EmptyTbl))))

#|
In the first, check with the first table('a "AAA" (all...)) , and the key is 'b
 return => (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) 'b) ,the same table and call the function again to the inside table of the first one
then , (Add 'b "B" (Add 'a "A" (EmptyTbl)))) 'b) => remove it, acttuly retrun only the table that inside witout add k v ( 'b "B") ,get (Add 'a "A" (EmptyTbl)))) 'b)
then add k v and call function for EmptyTbl , here return EmptyTbl ,then return for all operations of add recursion ...
finally we get (Add 'a "AAA" (Add 'a "A" (EmptyTbl)))).


|#


#|
what the main difficulties were,
how you overcame them, how much time you invested in solving it,
did you need to consult others

"The first question it's get a day it's not difficult but there new somthing it's list of list of number that the big problem
 in the first i'm not succed how to create the syntax for that ,
the second , two days it's more difficult than the first one ,but acttuly it's okay ,the lecture help me to solve and the tests that included in the homework.



|#