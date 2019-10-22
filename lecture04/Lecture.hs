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
