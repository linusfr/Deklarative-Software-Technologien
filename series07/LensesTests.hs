import           LensesAndPrisms

---------------------------------------
-- headLens
---------------------------------------
list = [1 .. 100000]

list2 = []

testHead = getterL headLens list

testHead2 = getterL headLens list2

---------------------------------------
-- headPrism
---------------------------------------
testPrism = getterP headPrism list

testPrism2 = getterP headPrism list2
