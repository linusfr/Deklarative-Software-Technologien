import           LensesAndPrisms

{-
1.
-}
---------------------------------------
-- headLens
---------------------------------------
list = [1 .. 10]

list2 = []

testHead = getterL headLens list

testHead2 = getterL headLens list2

---------------------------------------
-- headPrism
---------------------------------------
testPrism = getterP headPrism list

testPrism2 = getterP headPrism list2

---------------------------------------
-- testNthPrism
---------------------------------------
testNthPrismGet = (getterP (nthPrism' 5)) list

testNthPrismSet = ((setterP (nthPrism' 5)) list 10)

testNthPrismGet' = (getterP (nthPrism' 5)) list

testNthPrismSet' = ((setterP (nthPrism' 5)) list 10)

{-
2.
-}
---------------------------------------
-- liftTest
---------------------------------------
liftTest = (getterP (lift headLens)) list

liftTest' = (getterP (lift headLens)) []

{-
3.
-}
---------------------------------------
-- orList
---------------------------------------
boolList = [True, True, False, False, True]

boolList' = [False, False, False, False, False]

testOrList = orList boolList

testOrList' = orList boolList'

---------------------------------------
-- lookupFirst
---------------------------------------
greaterFive :: Int -> Bool
greaterFive x = x > 5

testLookupFirst = lookupFirst greaterFive list

testLookupFirst' = lookupFirst greaterFive [7 .. 20]

---------------------------------------
-- mapList
---------------------------------------
plusOne :: Int -> Int
plusOne i = i + 1

listA = [1 .. 10]

testMapList = mapList plusOne listA
