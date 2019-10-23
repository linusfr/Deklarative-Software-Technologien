module Lecture where

-- b) isPrefixOfString -> how to improve last rule
-- use && operator
isPrefixOfString :: String -> String -> Bool
isPrefixOfString [] (b:bs)         = True
isPrefixOfString (a:as) []         = False
-- this beautiful
isPrefixOfString (c1:c1s) (c2:c2s) = (c1 == c2) && isPrefixOfString c1s c2s

-- this not
--isPrefixOfString (a:as) (b:bs)
--  | a == b = isPrefixOfString as bs
--  | otherwise = False
-- unions :: [BoundingBox] -> BoundingBox
-- unions []     = Empty
-- unions (b:bs) = union b (unions bs)
-- Lambda Funktionen
-- Anonyme Funktionen
-- \x -> x+1
inc :: Int -> Int
inc x = x + 1

incAll :: [Int] -> [Int]
incAll l = map (\x -> x + 1) l

squareAll :: [Int] -> [Int]
squareAll l = map (\x -> x * x) l

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \a -> f (g a)

test :: [Int] -> [Int]
test l = map (compose inc square) l

compose' :: (b -> c) -> (a -> b) -> a -> c
compose' f g a = f (g a)

add :: Int -> (Int -> Int)
add x y = x + y

-- geCurryte
-- -> rechts assoziativ
-- main = (add 1) 2 -- add :: Int -> (Int -> Int)
-- 1 :: Int
-- 2 :: Int
-- add 1 :: Int -> Int
--
--
-- tanslate :: Float -> Float -> Graphic -> Graphic
-- translate dx dy g = map (translateForm dx dy) g
-- dx :: Float
-- dy :: Float
-- translateForm dx :: Float -> (Form -> Form)
-- translateForm dx dy :: Form -> Form
-- translateForm dx dy
--
--
data Maybe a
  = Nothing
  | Just a

-- Typkonstruktoren
-- Polymorpher Datentyp
data Either a b
  = Left a
  | Right b

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeHead' :: [a] -> Either String a
safeHead' []    = Left "head of empty list"
safeHead' (x:_) = Right x

test :: [Either Int String]
test = [Left 23, Right "a", Right "Hallo"]

lefts :: [Either a b] -> [a]
lefts []           = []
lefts (Left x:es)  = x : lefts es
lefts (Right _:es) = lefts es

isLeft :: Either a b -> Bool
isLeft Left _  = True
isLeft Right _ = False

fromLeft :: Either a b -> a
fromLeft (Left x)  = x
fromLeft (Right x) = x

lefts' :: [Either a b] -> [a]
lefts' l = map ? (filter isLeft l)

-- Tupel
-- Paar
-- Tripel
pair :: (Int, String)
pair = (23, "a")

triple :: (Int, String, Int [])
trile = (23, "a", [])

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y
