{-# LANGUAGE TemplateHaskell #-}

module Ranges where

type Range = Int -> Bool

pos :: Range
pos x = x >= 0

inRange :: Range -> Int -> Bool
inRange range = range

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
