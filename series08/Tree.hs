module Tree where

{-
Aufgabe 3 - Faltungen für Bäume
    Wir betrachten den folgenden Datentyp für binäre Bäume.

        data BinTree a = Empty | Node (BinTree a) a (BinTree a)

    Definieren Sie eine Faltung für den Datentyp BinTree. Gehen Sie dabei nach dem Schema aus der Vorlesung
    vor, um sich zu überlegen, wie die Faltung definiert ist.
-}
data BinTree a
  = Empty
  | Node (BinTree a) a (BinTree a)

{-
Beispiel Liste
    Konstruktoren
        - [] :: [a]
        - (:) :: a -> [a] -> [a]

    den zu faltenden Typen (hier [a]) durch einen neuen Typen ersetzen

    fold :: a -> (a -> b -> b) -> ...

    Dann braucht die Faltung noch die Datenstruktur, die Sie falten soll und gibt
    den neuen Typen zurück

    fold :: a -> (a -> b -> b) -> [a] -> b

Beispiel Baum

    data Tree a
        = Leaf a
        | Node (Tree a) (Tree a)

    nach dem Muster vom Listenbeispiel erhält man:

    foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b

    foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
    foldTree leaf _ (Leaf x) = leaf x
    foldTree leaf node (Node lt rt) = node (foldTree leaf node lt) (foldTree leaf node rt)

    leaves :: Tree a -> Int
    leaves (Leaf _) = 1
    leaves (Node lt rt) = leaves lt + leaves rt

    leaves' :: Tree a -> Int
    leaves' = foldTree (const 1) (+)

-}
test = 1

{-
    Definieren Sie die folgenden Funktionen mit Hilfe Ihrer
    Faltung.
        • Definieren Sie eine Funktion sumTree :: BinTree Int → Int, welche die Summe aller Einträge im Baum
        berechnet.
        • Definieren Sie eine Funktion values :: BinTree a → [a], die alle Einträge in einem Baum in einer Liste
        zurückliefert.
        • Definieren Sie eine Funktion mapTree :: (a → b) → BinTree a → BinTree b, die eine gegebene Funktion auf
        alle Werte im Baum anwendet.
-}
test = 2
