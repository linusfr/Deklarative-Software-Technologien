module List where

-------------------------------------------------------------
-- lookupKey
-------------------------------------------------------------
lookupKey :: (k -> k -> Bool) -> k -> [(k, v)] -> Maybe v
lookupKey _ _ [] = Nothing
lookupKey f k1 ((k2, v):ks)
  | f k1 k2 = Just v
  | otherwise = lookupKey f k1 ks

-- function for testing lookupKey
isIdentical :: Int -> Int -> Bool
isIdentical k1 k2
  | k1 == k2 = True
  | otherwise = False

-------------------------------------------------------------
-- andList
-------------------------------------------------------------
andList :: [Bool] -> Bool
andList [] = True
andList (b:bs)
  | b = andList bs
  | otherwise = False

-------------------------------------------------------------
-- zipWithList
-------------------------------------------------------------
zipWithList :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithList _ _ []          = []
zipWithList f (a:as) (b:bs) = (f a b) : (zipWithList f as bs)
