lookupKeyInt :: Int -> [(Int, v)] -> Maybe v
lookupKeyInt _ [] = Nothing
lookupKeyInt k (p:ps) =
  if fst p == k
    then snd p
    else lookupKeyInt k ps

fst :: (a, b) -> a
fst (x, _) = x

lookupKey :: (k -> k -> Bool) -> k -> [(k, v)] -> Maybe v
lookupKey eq k (p:ps) =
  if eq (fst p) k
    then Just (snd p)
    else lookupKeyInt k ps

lookupList :: (a -> Bool) -> [a] -> Maybe a
lookupList _ [] = Nothing
lookupList p (x:xs) =
  if p x
    then Just x
    else lookupList p xs

-- lookupKey' :: Int -> [(Int, v)] -> Maybe v
-- lookupKey' k l = lookupList foo l

-- map :: (a -> b) -> [a] -> [b]
--
test2 = map p [1, 2, 3]

-- p :: a -> b
-- lookupList' :: (a -> Bool) -> [a] -> Maybe a
-- l :: [(Int, v)]
-- foo :: a -> Bool
--

lookupKey' :: Int -> [(Int, v)] -> Maybe v
lookupKey' k l =  bar (lookupList (\p -> fst p == k) l)
-- lookupKey' k l = Maybe (snd lookupList (\p -> fst p == k) l)
-- lookupKey' k l = lookupList (foo k) l
-- same as
-- lookupKey' k l = lookupList (\p -> fst p == k) l

bar :: Maybe (Int, v) -> Maybe v
bar Nothing = Nothing
bar (Just p) = Just (snd p)

lookupKeyThree :: Eq k => (k -> k -> Bool) -> k -> [(k, v)] -> Maybe v
lookupKeyThree f givenKey l =
  let maybe = lookupList (\(listKey, v) -> f listKey givenKey) l
   in case maybe of
        Just (k, v) -> Just v
        Nothing     -> Nothing

lookupKeyThree :: Eq k => (k -> k -> Bool) -> k -> [(k, v)] -> Maybe v
lookupKeyThree f givenKey l =
  case lookupList (\(listKey, v) -> f listKey givenKey) l of
        Just (k, v) -> Just v
        Nothing     -> Nothing


-- partielle Applikation
-- man übergibt weniger Parameter als notwenig
-- lookupKey' :: Int -> [(Int, v)] -> Maybe (Int, v)
-- wir wollen
-- lookupKey' :: Int -> [(Int, v)] -> Maybe v

foo :: k -> (Int, v) -> Bool
foo k (x, _) == k 

allList :: (a-> Bool) -> [a] -> Bool
allList _ [] = True
allList p (x:xs) = p x && allList p xs

andList :: [Bool] -> Bool
andList l = allList id l
-- andList l = allList  (\b -> b) l 

id :: a -> a 
id a = a

















 