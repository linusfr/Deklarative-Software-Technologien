module IntList where

data IntList
  = Nil
  | Cons Int IntList
  deriving (Show)

intListLength :: IntList -> Int
intListLength Nil         = 0
intListLength (Cons _ is) = 1 + intListLength is

sumList :: IntList -> Int
sumList Nil         = 0
sumList (Cons i is) = i + sumList is

appendList :: IntList -> IntList -> IntList
appendList Nil l2         = l2
appendList Nil Nil        = Nil
appendList (Cons i is) l2 = Cons i (appendList is l2)

-- deletes n elements
-- if length(l) < n return l
dropList :: Int -> IntList -> IntList
dropList 0 l = l
dropList n (Cons j js)
  | n <= intListLength (Cons _ js) = dropList (n - 1) js
  | n > intListLength (Cons j js) = Cons j js

-- inserts n into the list after the fist non-null element
joinList :: Int -> IntList -> IntList
joinList n (Cons i (Cons j js)) = Cons i (Cons n (joinList n (Cons j js))) -- |l| > 1 --> need to insert n after every other element
joinList n l                    = l
