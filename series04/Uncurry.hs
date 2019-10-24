inc :: Int -> Int
inc x = x + 1

square :: Int -> Int
square x = x * x

exps2 = map (square . inc) [1, 2, 3, 4, 5]

exps3 = map (inc . square) [1, 2, 3, 4, 5]

add :: Int -> Int -> Int
add x y = x + y
--
-- Jede Funktion in Haskell nimmt nur ein Argument!
-- add kann auch wie folgt geschrieben werden:
-- add :: Int -> (Int -> Int)
-- Der Typkostruktor -> ist also rechts-assoziativ
--
-- add ist eine Funktion die einen Int nimmt
-- und eine Funktion vom Typ Int -> Int liefert
--
-- Funktionasanwendung ist links-assoziativ
-- add 1 2 steht für
-- (add 1) 2
