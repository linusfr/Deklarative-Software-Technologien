{-# LANGUAGE TemplateHaskell #-}

import           Ranges

test :: Range -> [Int]
test range = filter range [-100 .. 100]

testPos = test pos

testInRange = test (Ranges.inRange pos)

testWithout = test (Ranges.without pos)

testSchnitt = test ((/\) pos (Ranges.without pos))

testVereinigung = test ((\/) pos (Ranges.without pos))

testShift = test (shift (\x -> x == 1) 1)

----------------------------------------------------
type Stringrange = Int -> String

lol :: Stringrange
lol x = "haltiefres"

testString :: Stringrange -> Stringrange
testString str = str

testString2 :: Stringrange -> Int -> String
testString2 str x = "ist das selbe nur einmal direkt"

testDirekt = testString lol 7

testIndirekt = testString2 lol 7
----------------------------------------------------
