module List where

-------------------------------------------------------------
-- lookupKey - version 1
-------------------------------------------------------------
lookupKeyOne :: (k -> k -> Bool) -> k -> [(k, v)] -> Maybe v
lookupKeyOne _ _ [] = Nothing
lookupKeyOne f k1 ((k2, v):ks)
  | f k1 k2 = Just v
  | otherwise = lookupKeyOne f k1 ks

-- function for testing lookupKey
isIdentical :: Int -> Int -> Bool
isIdentical k1 k2
  | k1 == k2 = True
  | otherwise = False

-------------------------------------------------------------
-- lookupKey - version 2
-------------------------------------------------------------
lookupKeyTwo :: (k -> k -> Bool) -> k -> [(k, v)] -> Maybe v
lookupKeyTwo f k l =
  let listLength =
        length (filter (\x -> isIdentical k x) (map (\(k, v) -> k) l))
   in let keyExists = listLength == 1
       in if keyExists
            then Just
                   ((map
                       (\(listK, v) -> v)
                       (filter (\(listK, v) -> listK == k) l)) !!
                    0)
            else Nothing

lookupList :: (a -> Bool) -> [a] -> Maybe a
lookupList _ [] = Nothing
lookupList f (a:as)
  | f a = Just a
  | otherwise = lookupList f as

-------------------------------------------------------------
-- andList - version 1
-------------------------------------------------------------
andListOne :: [Bool] -> Bool
andListOne [] = True
andListOne (b:bs)
  | b = andListOne bs
  | otherwise = False

-------------------------------------------------------------
-- andList - version 2
-------------------------------------------------------------
andListTwo :: [Bool] -> Bool
andListTwo bs = allList (\b -> b) bs

allList :: (a -> Bool) -> [a] -> Bool
allList _ [] = True
allList f (a:as)
  | f a = allList f as
  | otherwise = False

-------------------------------------------------------------
-- zipWithList
-------------------------------------------------------------
zipWithList :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithList _ _ []          = []
zipWithList f (a:as) (b:bs) = (f a b) : (zipWithList f as bs)
