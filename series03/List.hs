module List where

-- a) appendList
appendList :: [a] -> [a] -> [a]
appendList [] b     = b
appendList (a:as) b = a : (appendList as b)

-- b) isPrefixOfString
isPrefixOfString :: String -> String -> Bool
isPrefixOfString [] (b:bs) = True
isPrefixOfString (a:as) [] = False
isPrefixOfString (a:as) (b:bs)
  | a == b = isPrefixOfString as bs
  | otherwise = False

-- c) allList
allList :: (a -> Bool) -> [a] -> Bool
allList _ [] = True
allList f (a:as)
  | f a = allList f as
  | otherwise = False

reverseList :: [a] -> [a]
reverseList []     = []
reverseList (a:as) = appendList (reverseList as) (a : [])

-- d) lookupList
lookupList :: (a -> Bool) -> [a] -> Maybe a
lookupList _ [] = Nothing
lookupList f (a:as)
  | f a = Just a
  | otherwise = lookupList f as
