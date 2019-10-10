module IntList where

data IntList
  = Nil
  | Cons Int IntList
  deriving (Show)

intListLength :: IntList -> Int
intListLength Nil = 0
intListLength (Cons _ is) = 1 + intListLength is

sumList :: IntList -> Int
sumList Nil = 0
sumList (Cons i is) = i + sumList is

appendList :: IntList -> IntList -> IntList
appendList Nil (Cons i is) = Cons i (appendList Nil is)
appendList Nil Nil = Nil
appendList (Cons i is) l2 = Cons i (appendList is l2)

-- deletes n elements
-- if length(l) < n return l
dropList :: Int -> IntList -> IntList
dropList n Nil = Nil
dropList 0 l = l
dropList n (Cons j js)
  | n <= intListLength (Cons j js) = dropList (n - 1) js
  | n > intListLength (Cons j js) = Cons j js

-- inserts n into the list after the fist non-null element
joinList :: Int -> IntList -> IntList
joinList n Nil = Nil -- |l| = 0
joinList n (Cons i Nil) = Cons i Nil -- |l| = 1
joinList n (Cons i (Cons j js)) = Cons i (Cons n (joinList n (Cons j js))) -- |l| > 1 --> need to insert n after every other element

-- test
list :: IntList
list = Nil

list2 :: IntList
list2 = Cons 20 list

list3 :: IntList
list3 = Cons 10 list2

list4 :: IntList
list4 = Nil

list5 :: IntList
list5 = Cons 40 list4

list6 :: IntList
list6 = Cons 30 list5

theSum :: Int
theSum = sumList list3

theLength :: Int
theLength = intListLength list6

newList :: IntList
newList = joinList 0 list6

newList2 :: IntList
newList2 =
  joinList 0 (appendList (appendList list3 list6) (appendList list3 list6))
