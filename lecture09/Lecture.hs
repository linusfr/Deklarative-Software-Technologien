module Lecture where

type Prob = Float

-- polymorpher (wegen dem a) typsynonym
type Dist a = [(a, Prob)]

certainly :: a -> Dist a
certainly a = [(a, 1)]

uniform :: [a] -> Dist a
uniform l = map (\x -> (x, p)) l
  where
    p = 1 / fromIntegral (length l)

{-
p = Wahrscheinlichkeit eines Ereignisses
fromIntegral rechnet Int (von length ausgegeben) zu FLoat um,
da wir mir Fließkommazahlen arbeiten möchten
-}
coin :: Dist Bool
coin = uniform [False, True] -- wir erhalten eine Verteilung, die 50% True oder False zuordnet

--
-- Operator: man erhält ein Prädikat, die ein Ereignis beschreibt, das man beobachten möchte (e..g dass ein Würfel 4 zeigt),
-- und eine Verteilung, und soll die Wahrscheinlichkeit davon rausgeben, dass das Prädikat erfüllt wird
--
--one way to write this:
-- (??) :: (a -> Bool) -> Dist a -> Prob
-- pred ?? dist = sum (map snd (filter (pred . fst) dist))
--
-- alternative way to write this using piping (|>) (same as &&)
(??) :: (a -> Bool) -> Dist a -> Prob
pred ?? dist = dist |> filter (pred . fst) |> map snd |> sum

-- WH: piping
(|>) :: a -> (a -> b) -> b
x |> f = f x

------
-- example dice
-------
data Side
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  deriving (Show, Eq) -- in order to be able to use == we need Eq

die :: Dist Side
die = uniform [One, Two, Three, Four, Five, Six]

isSix :: Side -> Bool
isSix Six = True
isSix _   = False

-- mapDist :: (a -> b) -> Dist a -> Dist b
-- mapDist _ [] = []
-- mapDist f ((x,p):xs) = (f x, p) : mapDist f xs
-- schöner
-- mapDist :: (a -> b) -> Dist a -> Dist b
-- mapDist f dist = map apply dist
--   where
--     apply (x, p) = (f x, p)
-- mit uncurry
mapDist :: (a -> b) -> Dist a -> Dist b
mapDist f dist = map (uncurry apply) dist
  where
    apply x p = (f x, p)

sideToInt :: Side -> Int
sideToInt One   = 1
sideToInt Two   = 2
sideToInt Three = 3
sideToInt Four  = 4
sideToInt Five  = 5
sideToInt Six   = 6

-- cross :: (a -> b -> c) -> [a] -> [b] -> [c]
-- cross f [] _      = []
-- cross f (x:xs) ys = map (f x) ys ++ cross f xs ys
cross :: (a -> b -> c) -> [a] -> [b] -> [c]
cross f xs ys = concat (map (\x -> map (f x) ys) xs)

combDist :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
combDist f dist1 dist2 = cross comb dist1 dist2
  where
    comb (x, p) (y, p) = (f x y, p * q)

dieInt :: Dist Int
dieInt = mapDist sideToInt die

twoDice :: Dist Int
twoDice = combDist (+) dieInt dieInt
