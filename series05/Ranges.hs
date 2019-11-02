{-# LANGUAGE TemplateHaskell #-}

module Ranges where

--------------------------------------------
-- range
--------------------------------------------
type Range = Int -> Bool

--------------------------------------------
-- first block
--------------------------------------------
pos :: Range
pos x = x >= 0

inRange :: Range -> Int -> Bool
inRange range = range

--------------------------------------------
-- second block
--------------------------------------------
without :: Range -> Range
without range x = not (range x)

(/\) :: Range -> Range -> Range
(/\) r1 r2 x = (r1 x) && (r2 x)

-- (/\) r1 r2 x = (\x -> (r1 x) && (r2 x))
--
(\/) :: Range -> Range -> Range
(\/) r1 r2 x = (r1 x) || (r2 x)

--(/\) r1 r2 x = (\x -> (r1 x) || (r2 x))
--
shift :: Range -> Int -> Range
shift range inputX outputFunctionX = (range (outputFunctionX - inputX))

--------------------------------------------
-- third block
--------------------------------------------
-- Die Funktion from :: Int → Range liefert einen Zahlenbereich, der mit der übergebenen Zahl beginnt.
-- Definieren Sie from mittels pos.
from :: Int -> Range
from shiftBy outputFunctionX = shift pos shiftBy outputFunctionX

-- Die Funktion to :: Int → Range liefert einen Zahlenbereich, der mit der übergebenen Zahl endet. Definieren
-- Sie to mittels from.
to :: Int -> Range
to endAt outputFunctionX = outputFunctionX <= endAt && from 0 outputFunctionX

-- Die Funktion fromTo :: Int → Int → Range liefert einen Zahlenbereich zwischen zwei Zahlen (inklusive der
-- Grenzen). Definieren Sie fromTo mittels from und to.
fromTo :: Int -> Int -> Range
fromTo start end = from start /\ to end
