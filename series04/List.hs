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
isIdentical :: Eq k => k -> k -> Bool
isIdentical x y = x == y

-------------------------------------------------------------
-- lookupKey - version 2
-------------------------------------------------------------
lookupKeyTwo :: Eq k => (k -> k -> Bool) -> k -> [(k, v)] -> Maybe v
lookupKeyTwo f k l =
  let maybeKey = lookupList (\a -> f k a) (map (\(k, v) -> k) l)
   in if maybeKey == Nothing
        then Nothing
        else Just
               ((map (\(listK, v) -> v) (filter (\(listK, v) -> listK == k) l)) !!
                0)

lookupList :: (a -> Bool) -> [a] -> Maybe a
lookupList _ [] = Nothing
lookupList f (a:as)
  | f a = Just a
  | otherwise = lookupList f as

-------------------------------------------------------------
-- lookupKey - version 3
-------------------------------------------------------------
-- lookupKeyThree :: Eq k => (k -> k -> Bool) -> k -> [(k, v)] -> Maybe v
-- lookupKeyThree f givenKey l =
--   let maybe = lookupList (\(listKey, v) -> f listKey givenKey) l
--    in case maybe of
--         Just (k, v) -> Just v
--         Nothing     -> Nothing
lookupKeyThree :: Eq k => (k -> k -> Bool) -> k -> [(k, v)] -> Maybe v
lookupKeyThree f givenKey l =
  let maybe = lookupList (\(listKey, v) -> f listKey givenKey) l
   in if maybe == Just (k, v)
        then Just v
        else Nothing

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
