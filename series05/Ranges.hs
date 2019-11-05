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
pos  x= x >= 0
-- pos = (>= 0)

inRange :: Range -> Int -> Bool
inRange range = range

--------------------------------------------
-- second block
--------------------------------------------
without :: Range -> Range
without range = not . range

(/\) :: Range -> Range -> Range
r1 /\ r2 = \x -> r1 x && r2 x

-- (r1 \/ r2) x = r1 x || r2 x
(\/) :: Range -> Range -> Range
r1 \/ r2 = \x -> r1 x || r2 x

shift :: Range -> Int -> Range
shift range inputX outputFunctionX = range (outputFunctionX - inputX)

-- inRange (shift pos 2) 1
-- ==
-- (shift pos 2) 1
-- == 
-- pos (2-2)
-- == 
-- 0
-- ==
-- True

--------------------------------------------
-- third block
--------------------------------------------
from :: Int -> Range
from = shift pos

to :: Int -> Range
to endAt = pos /\ without (from (endAt + 1))

fromTo :: Int -> Int -> Range
fromTo start end = from start /\ to end
