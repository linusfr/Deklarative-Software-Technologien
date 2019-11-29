module Tree where

{-
Aufgabe 3 - Faltungen für Bäume
    Wir betrachten den folgenden Datentyp für binäre Bäume.

        data BinTree a = Empty | Node (BinTree a) a (BinTree a)

    Definieren Sie eine Faltung für den Datentyp BinTree. Gehen Sie dabei nach dem Schema aus der Vorlesung
    vor, um sich zu überlegen, wie die Faltung definiert ist.
-}
{-
Beispiel Liste

data List = []
          | (:)
    Konstruktoren
        - [] :: [a]
        - (:) :: a -> [a] -> [a]

    den zu faltenden Typen (hier [a]) durch einen neuen Typen ersetzen

    foldTree :: a -> (a -> b -> b) -> ...

    Dann braucht die Faltung noch die Datenstruktur, die Sie falten soll und gibt
    den neuen Typen zurück

    foldTree :: a -> (a -> b -> b) -> [a] -> b

Beispiel Baum

    data Tree a
        = Leaf a
        | Node (Tree a) (Tree a)

    nach dem Muster vom Listenbeispiel erhält man:

    foldTreeTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b

    foldTreeTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
    foldTreeTree leaf _ (Leaf x) = leaf x
    foldTreeTree leaf node (Node lt rt) = node (foldTreeTree leaf node lt) (foldTreeTree leaf node rt)

    leaves :: Tree a -> Int
    leaves = foldTreeTree (const 1) (+)

-}
data BinTree a
  = Empty
  | Node (BinTree a) a (BinTree a)
  deriving (Show)

{-
Konstruktoren
    - Empty :: BinTree a
    - Node :: BinTree a -> a -> BinTree a -> BinTree a

    =>

    - Empty :: b
    - Node :: b -> a -> b -> b
-}
foldTree :: b -> (b -> a -> b -> b) -> BinTree a -> b
foldTree empty _ Empty = empty
foldTree empty node (Node lt value rt) =
  node (foldTree empty node lt) value (foldTree empty node rt)

{-
        • Definieren Sie eine Funktion sumTree :: BinTree Int → Int, welche die Summe aller Einträge im Baum
        berechnet.
-}
sumTree :: BinTree Int -> Int
sumTree = foldTree 0 (\lTree value rTree -> lTree + value + rTree)

{-
        • Definieren Sie eine Funktion values :: BinTree a → [a], die alle Einträge in einem Baum in einer Liste
        zurückliefert.
-}
values :: BinTree a -> [a]
values = foldTree [] (\lTree value rTree -> lTree ++ [value] ++ rTree)

{-
        • Definieren Sie eine Funktion mapTree :: (a → b) → BinTree a → BinTree b, die eine gegebene Funktion auf
        alle Werte im Baum anwendet.
-}
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f = foldTree Empty (\lTree value rTree -> Node lTree (f value) rTree)
