import           List

-------------------------------------------------------------
-- lookupKey
-------------------------------------------------------------
keyValueList = [(3, 4), (5, 7), (8, 4), (9, 10)]

lookupKeyTest = lookupKey isIdentical 8 keyValueList

-------------------------------------------------------------
-- andList
-------------------------------------------------------------
testListBool1 = [True, True, True, True, True, True, True, False]

testListBool2 = [True, True, True, True, True, True, True, True]

andListTest1 = andList testListBool1

andListTest2 = andList testListBool2

-------------------------------------------------------------
-- zipWithList
-------------------------------------------------------------
testZipList1 = [2, 5, 4, 7, 8, 9]

testZipList2 = [2, 6, 4, 7, 8, 10]

test = zipWithList (\x y -> (x, y)) testZipList1 testZipList2
