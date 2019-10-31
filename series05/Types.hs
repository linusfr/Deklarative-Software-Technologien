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

-- same as b1 && b2
-- => && returns a Bool
--
-----------------------------------------------------------
-- 3.
-----------------------------------------------------------
exps3 = map (\b1 b2 -> b1 && b2)

-- => [Bool]
--
-----------------------------------------------------------
-- 4.
-----------------------------------------------------------
exps4 = map (\b1 b2 -> b1 && b2) [False, True]

-- => [Bool -> Bool]
-- => liefert Funktionen zurück, welche wiederum an ein neues map übergeben werden können.
--
-----------------------------------------------------------
-- 5.
-----------------------------------------------------------
exps5 = (\f -> f False)

-- (Bool -> t)
-- => Funktion definiert den Ausgabetyp
--
-----------------------------------------------------------
-- 6.
-----------------------------------------------------------
exps6 = map (\f -> f False)

-- map (\f -> f False) [(\b -> True), (\b -> False), (\b -> True), (\b -> False)]
-- exps6 :: [Bool -> b] -> [b]
--
-- [t]
-- => Funktionen definieren den Typ der Liste
--
-----------------------------------------------------------
-- 7.
-----------------------------------------------------------
exps7 = map (\f -> f False) (map (\b1 b2 -> b1 && b2) [False, True])

exps7_a = (map (\b1 b2 -> b1 && b2) [False, True])

-- => exps7_a returns a function list which is input for exps7_b
exps7_b = map (\f -> f False) [(\b1 -> b1 && False), (\b1 -> b1 && True)]

-- => returns [Bool]
--
-----------------------------------------------------------
-- 8.
-----------------------------------------------------------
exps8 = [(False, True), (True, True)]

-- => returns [(Bool, Bool)]
--
-----------------------------------------------------------
-- 8.
-----------------------------------------------------------
-- curry :: ((a, b) -> c) -> a -> b -> c
-- curry f a b = f (a, b)
--
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- uncurry f (a, b) = f a b
exps9 = uncurry (&&)

-- (&&) :: Bool -> Bool -> Bool
-- uncurry :: (&&) <==> Bool -> Bool -> Bool -> (a, b) -> c
-- uncurry && (a, b) = a && b
exps9_b = exps9 (True, False)

-- exps9_a verwendet intern && auf die Parameter
-- aber es gibt so nur "einen" Parameter
--
-----------------------------------------------------------
-- 9.
-----------------------------------------------------------
exps10 = filter (uncurry (&&))

-- => Ausgabe entspricht dem typen der Liste die übergeben wird, sie muss allerdings aus tupeln bestehen für uncurry(&&)
--
-----------------------------------------------------------
-- 10.
-----------------------------------------------------------
exps10 = filter (uncurry (&&)) [(False, True), (True, True)]
-- => [Bool, Bool]
