{-# LANGUAGE TemplateHaskell #-}

import           Ranges

test :: Range -> [Int]
test range = filter range [-100 .. 100]

--------------------------------------------
-- first block
--------------------------------------------
testPos = test pos

testInRange = test (Ranges.inRange pos)

--------------------------------------------
-- second block
--------------------------------------------
testWithout = test (Ranges.without pos)

testSchnitt = test ((/\) pos (Ranges.without pos))

testVereinigung = test ((\/) pos (Ranges.without pos))

testShift = test (shift (\x -> x == 1) 1)

--------------------------------------------
-- third block
--------------------------------------------
testFrom = test (from (-50))

toTest = test (to 50)
