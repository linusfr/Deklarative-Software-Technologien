{-# LANGUAGE TemplateHaskell #-}

-- PART 01 - Types
-----------------------------------------------------------
-- 1.
-----------------------------------------------------------
exps1 = [False, True]

-- [Bool]
-- => List of Bools
--
-----------------------------------------------------------
-- 2.
-----------------------------------------------------------
exps2 = (\b1 b2 -> b1 && b2)

-- Bool -> Bool -> Bool
-- => returns a function which takes two bools
-- and returns another
--
-----------------------------------------------------------
-- 3.
-----------------------------------------------------------
exps3 = map (\b1 b2 -> b1 && b2)

-- [Bool] -> [Bool -> Bool]
-- => creates an array of functions taking two bools and returning another
--
-----------------------------------------------------------
-- 4.
-----------------------------------------------------------
exps4 = map (\b1 b2 -> b1 && b2) [False, True]

-- [Bool -> Bool]
-- => returns a function array which takes one bool and returns another
--
-----------------------------------------------------------
-- 5.
-----------------------------------------------------------
exps5 = (\f -> f False)

-- (Bool -> t) -> t
-- => the function passed to the lambda defines the output type
-- which in turn gets returned by the lambda
--
-----------------------------------------------------------
-- 6.
-----------------------------------------------------------
exps6 = map (\f -> f False)

-- [Bool -> b] -> [b]
-- => takes a function array. The function defines the return type
--
-----------------------------------------------------------
-- 7.
-----------------------------------------------------------
exps7 = map (\f -> f False) (map (\b1 b2 -> b1 && b2) [False, True])

-- [Bool]
-- => right map creates an array of functions taking one bool and
-- returning another which is passed into the left map
-- which passes False into the created Functions
-- returning a Bool Array
--
-----------------------------------------------------------
-- 8.
-----------------------------------------------------------
exps8 = [(False, True), (True, True)]

-- [(Bool, Bool)]
--
-----------------------------------------------------------
-- 9.
-----------------------------------------------------------
exps9 = uncurry (&&)

-- curry :: ((a, b) -> c) -> a -> b -> c
-- curry f a b = f (a, b)
--
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- uncurry f (a, b) = f a b
--
-- (&&) :: Bool -> Bool -> Bool
-- uncurry :: (&&) <==> Bool -> Bool -> Bool -> (a, b) -> c
-- uncurry && (a, b) = a && b
--
exps9_b = exps9 (True, False)

-- (Bool, Bool) -> Bool
-- takes a Bool pair and returns a bool
--
-----------------------------------------------------------
-- 10.
-----------------------------------------------------------
exps10 = filter (uncurry (&&))

-- [(Bool, Bool)] -> [(Bool, Bool)]
-- filter needs a list of bool pairs for && and returns
-- another list of bool pairs
--
-----------------------------------------------------------
-- 11.
-----------------------------------------------------------
exps11 = filter (uncurry (&&)) [(False, True), (True, True)]-- [(Bool, Bool)]
-- same as 10, but the neccessary pairs are given, returns only the
-- results of the filtering
-- [(True, True)]
