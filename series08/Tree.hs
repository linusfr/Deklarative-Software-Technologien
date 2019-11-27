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

-- fold :: BinTree a -> (a -> a -> a -> b) -> b
-- fold Empty _                    = 0
-- fold (Node lTree value rTree) f = f value (fold lTree) (fold rTree)
-- fold :: Leaf     -> Node          -> Tree   -> value
-- fold :: (a -> b) -> (b -> b -> b) -> Tree a -> b
-- fold leaf node (Leaf x) = leaf x
-- fold leaf node (Node left right) =
--   node (fold leaf node left) (fold leaf node right)
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
