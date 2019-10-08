{-# LANGUAGE TemplateHaskell #-}

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
appendList Nil (Cons i is) = Cons i (appendList Nil is)
appendList Nil Nil         = Nil
appendList (Cons i is) l2  = Cons i (appendList is l2)

-- deletes n elements
-- if length(l) < n return l
dropList :: Int -> IntList -> IntList
dropList n (Cons j js)
  | n < intListLength (Cons j js) = Cons j js
  | n >= intListLength (Cons j js) = dropList (n - 1) js
  | js == Nil = Nil

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
-- newList = appendList list3 list6
newList = dropList 1 list6
