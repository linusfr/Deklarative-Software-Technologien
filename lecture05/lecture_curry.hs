curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

-- isomorphismus
-- eine funktion ist curried und uncurried gleich in der funktionalität
--
sumPairs :: [(Int, Int)] -> [Int]
sumPairs = map foo

-- sumPairs = map (\(x, y) -> x + y)
--
foo :: (Int, Int) -> Int
foo = uncurry (+)

--foo p = uncurry bar p
--
bar :: a -> b -> c
bar x y = x + y
