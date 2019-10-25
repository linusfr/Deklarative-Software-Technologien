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
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- lookupItem :: (Int -> Bool) -> [Int] -> [Int]
-- lookupItem _ [] = []
-- lookupItem f (i:is)
--   | f i = [i]
--   | otherwise = lookupItem f is
-- isMatching :: (Int -> Int -> Bool) -> Int -> [Int] -> [Int]
-- isMatching f x l = lookupItem (\y -> (f x y)) l
-- exps4 = isMatching (\x y -> y < x) 10 [1 .. 20]
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- lookupItem :: (Int -> Bool) -> [Int] -> Int
-- lookupItem _ [] = 0
-- lookupItem f (i:is)
--   | f i = i
--   | otherwise = lookupItem f is
-- isMatching :: (Int -> Int -> Bool) -> Int -> [Int] -> Int
-- isMatching f x l = (filter (\y -> y == (lookupItem (\y -> (f x y)) l)) l) !! 0
-- exps4 = isMatching (\x y -> x == y) 10 [1 .. 20]
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- lookupItem :: (Int -> Bool) -> [Int] -> Int
-- lookupItem _ [] = 0
-- lookupItem f (i:is)
--   | f i = i
--   | otherwise = lookupItem f is
-- ---------------------------------------------------------------------------
-- isMatching :: (Int -> Int -> Bool) -> Int -> [(Int, String)] -> Int
-- isMatching f x l =
--   (filter (\y -> y == (lookupItem (\y -> (f x y)) l)) (map (\(k, v) -> k) l)) !!
--   0
-- exps4 =
--   isMatching
--     (\x y -> x == y)
--     10
--     [(1, "a"), (4, "g"), (9, "h"), (4, "c"), (5, "d"), (10, "Bingo!")]
exps5 =
  (map
     (\(k, v) -> v)
     (filter
        (\x ->
           x ==
           ((map
               (\(k, v) -> (k, v))
               [ (1, "a")
               , (4, "g")
               , (9, "h")
               , (4, "c")
               , (5, "d")
               , (10, "Bingo!")
               ]) !!
            5 ---------------------------------------------------------------------------
            ))
        [(1, "a"), (4, "g"), (9, "h"), (4, "c"), (5, "d"), (10, "Bingo!")] -------------------------------------------------------------------------
      )) !!
  0-- lookupKeyTwo :: (k -> k -> Bool) -> k -> [(k, v)] -> Maybe v
-- lookupKeyTwo f y ((k, v):bs) =
--   if (lookupList (\x -> (f y x)) (map (\(k, v) -> k) ((k, v) : bs))) == Just k
--     then map (\(k, v) -> Just v) (filter (\k -> k == y) [(k, v)])
--     else Nothing
-- keyEquals :: k -> k -> Bool
-- keyEquals k1 k2 = k1 == k2
-- lookupList :: (a -> Bool) -> [a] -> Maybe a
-- lookupList _ [] = Nothing
-- lookupList f (a:as)
--   | f a = Just a
--   | otherwise = lookupList f as
