-- PatternMatching
workday2 :: Weekday -> Bool
workday Saturday = False
workday Sunday   = False
workday _        = True

-- case-Ausdruck (andere Art Pattern Matching zu schreiben)
workday2 :: Weekday -> Bool
workday2 w =
  case w of
    Saturday -> False
    Sunday   -> False
    _        -> True

-- Guards (quasi wie switch case in Java, kein = vor erstem guard)
even :: Int -> Bool
even n
  | n `mod` 2 == 0 = True
  | n == 23 = True
  | otherwise = False

-- otherwise is previously defined as such:
-- otherwise :: Bool
-- otherwise = True
-- Rekursion
data IntList
  = Nil --nihil -- konstruiert eine leere Liste
  | Cons Int IntList --  Liste konstruieren, bei der man vorne appended und wieder neue Liste erhält

list1 :: IntList
list1 = Nil

list2 :: IntList
list2 = Cons 42 Nil -- hier fügen wir 42 hinzu als Element

list3 :: IntList
list3 = Cons 1 (Cons 2 Nil)

intListLength :: IntList -> Int
intListLength Nil         = 0
intListLength (Cons _ is) = 1 + intListLength is --recursive call, +1 because we lost one Cons

--function that shows every second element in the list
everySecond :: IntList -> IntList
everySecond Nil                  = Nil -- check if list is empty
everySecond (Cons _ Nil)         = Nil -- check if list has a single element
everySecond (Cons i (Cons j js)) = Cons j (everySecond js) -- check if list has minimum of two elements, returns new list of second elements

-- Baum
data IntTree
  = Empty
  | Node IntTree Int IntTree

tree1 :: IntTree
tree1 = Empty

tree2 :: IntTree
tree2 = Node Empty 42 Empty

tree3 :: IntTree
tree3 = Node (Node Empty 2 Empty) 42 Empty

-- Suchbaum
-- find :: IntTree -> Int -> Bool
-- find _ Empty = False
-- find n (Node lt m rt) = n == m || find n lt || find n rt find
{- In Java:
    || Kurzschlussoperatoren --> schaut sich nicht mehr die rechte Seite an

- In Haskell: call-by name or call-by need --> alle Operationen verhalten sich wie Kurzschlussoperatoren

-}
--(lt < m < rt --> man kann je nach wert von m links oder rechts rum gehen)
find :: IntTree -> Int -> Bool
find _ Empty = False
find n (Node lt m rt)
  | n == m = True
  | n < m = find n lt
  | n > m = find n rt

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
