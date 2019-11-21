import           Data.Typeable
import           LensesAndPrisms

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

---------------------------------------
-- liftTest
---------------------------------------
liftTest = (getterP (lift headLens)) list

liftTest' = getValue ((getterP (lift headLens)) [])

getValue :: Maybe v -> Constraint x
getValue (Just v) = (show (typeOf v))
