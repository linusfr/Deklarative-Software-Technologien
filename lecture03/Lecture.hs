module Lecture where

-------------------------------------------------------------------------------------
-- repetition lecture02
-------------------------------------------------------------------------------------
absAll :: IntList -> IntList -- in einer Liste von jedem Element den Absolutbetrag |x| berechnen und in neue Liste schreiben
absAll Nil         = Nil
absAll (Cons i is) = Cons (abs i) (absAll is)

list11 :: IntList
list11 = Cons (-1) (Cons (-2) (Cons (-23) Nil))

incAll :: IntList -> IntList -- jedes Element um eins erhöhen
incAll Nil         = Nil
incAll (Cons i is) = Cons (i + 1) (incAll is)

squareAll :: IntList -> IntList -- Elemente quadrieren
squareAll Nil         = Nil
squareAll (Cons i is) = Cons (i * i) (squareAll is)

listl1 :: IntList
list11 = Cons (-1) (Cons (-2) (Cons (-23) Nil))

-------------------------------------------------------------------------------------
-- higher order functions
-------------------------------------------------------------------------------------
map :: (Int -> Int) -> IntList -> Intlist
map _ Nil         = Nil
map f (Cons i is) = Cons (f i) (map f is)

absAll' :: IntList -> IntList
absAll' l = map abs l

incAll' :: IntList -> IntList
incAll' l = map inc listl1

squareAll' :: IntList -> IntList
squareAll' l = map square l

keepOnlyEven :: IntList -> IntList
keepOnlyEven Nil = Nil
keepOnlyEven (Cons i is) =
  if mod i 2 == 0
    then Cons i (keepOnlyEven is)
    else keepOnlyEven is

keepLess5 :: IntList -> IntList
keepLess5 Nil = Nil
keepLess5 (Cons i is) =
  if i < 5
    then Cons i (keepLess5 is)
    else keepLess5 is

filterList :: (Int -> Bool) -> IntList -> IntList
filterList _ Nil = Nil
filterList f (Cons i is) =
  if f i
    then Cons i (filterList f is)
    else filterList f is

-------------------------------------------------------------------------------------
-- generics
-- parametrischer polymorphismus
-------------------------------------------------------------------------------------
data List
  = Nil
  | Cons Int List

data List
  = Nil
  | Cons String List

data List a
  = Nil
  | Cons a (List a)

  list1 :: List Int
  list1 = Cons 1 (Cons 2 Nil)

  list2 :: List String
  list2 = Cons "a" (Cons "b" Nil)

  mapList :: (a -> b) -> List a -> List b
  mapList _ Nil = Nil 
  mapList f (Cons i is) = Cons (f i) (mapList f is)

-------------------------------------------------------------------------------------
-- lists
-------------------------------------------------------------------------------------

--   data [a] =
--     []
--     | a : [a]

list2' [String]
list2' = "a" : ("b" : [])

list1' :: [Int]
list1' = 1 (2 : [])

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (i : is) = f i : map f is

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs) = 
    if p x
        then x : filter p xs
        else filter p xs

length :: [a] -> Int
length [] = 0
length (_ : is) = 1 + length is

head :: [a] -> a
head (x:_) = x

data Maybe a
    = Nothing
    | Just a

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

test xs = 
    case safeHead xs of 
        Nothing -> "Fehler"
        Just x -> show x