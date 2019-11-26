import           Prelude hiding (flip, foldl)

flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b

{-
• Die Funktion mapList :: (a → b) → [a] → [b], die eine Funktion auf alle Elemente einer Liste anwendet.
-}
mapList :: (a -> b) -> [a] -> [b]
mapList f = foldr (\new old -> f new : old) []

-- Implementierung von MapList mit foldl mithilfe von flip
mapList' :: (a -> b) -> [a] -> [b]
mapList' f = foldl (flip (\new old -> f new : old)) []

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc []     = acc
foldl f acc (x:xs) = foldl f acc xs

------------------------------------------------------------------
-- neues Thema
------------------------------------------------------------------
{-
DSL
    - domain specific language
        - sprache für einen spezifischen Zweck
            - R für Datenverarbeitung
            - ursprünglich js für Browser
    - general purpose language
        - kann alles aber nix richtig

    - Dokumentation
    - Compiler / Interpreter
        - Interpreter sind leichter zu schreiben
        - Compiler sind schneller
    - IDE
        - Code Highlighting etc.
    - Debugger
        - wtf is wrong?

EDSL
    - embedded domain specific language
    - deep embedding / shallow embedding
-}
{-
probabilistische Programmiersprache
    - wahrscheinlichkeitsbasierte Sprache
-}
type Prob = Float

-- polymorpher (wegen dem a) typsynonym
type Dist a = [(a, Prob)]

certainly :: a -> Dist a
certainly a = [(a, 1)]

uniform :: [a] -> Dist a
uniform l = map (\x -> (x, p)) l
  where
    p = 1 / fromIntegral (length l)
