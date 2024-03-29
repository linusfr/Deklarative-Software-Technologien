import           List

-------------------------------------------------------------
-- lookupKey
-------------------------------------------------------------
keyValueList = [(3, 4), (5, 7), (8, 4), (9, 10)]

anotherKeyValueList =
  [(1, "a"), (4, "g"), (9, "h"), (4, "c"), (5, "d"), (10, "Bingo!")]

-- version 1
lookupKeyTest1 = lookupKeyOne isIdentical 8 keyValueList

lookupKeyTest2 = lookupKeyOne isIdentical 4 keyValueList

-- version 2
lookupKeyTest3 = lookupKeyTwo isIdentical 9 anotherKeyValueList

lookupKeyTest4 = lookupKeyTwo isIdentical 7 anotherKeyValueList

--version 3
lookupKeyTest5 = lookupKeyThree isIdentical 9 anotherKeyValueList

lookupKeyTest6 = lookupKeyThree isIdentical 7 anotherKeyValueList

-------------------------------------------------------------
-- andList
-------------------------------------------------------------
testListBool1 = [True, True, True, True, True, True, True, False]

testListBool2 = [True, True, True, True, True, True, True, True]

-- version 1
andListTest1 = andListOne testListBool1

andListTest2 = andListOne testListBool2

-- version 2
andListTest3 = andListTwo testListBool1

andListTest4 = andListTwo testListBool2

-------------------------------------------------------------
-- zipWithList
-------------------------------------------------------------
testZipList1 = [2, 5, 4, 7, 8, 9]

testZipList2 = [2, 6, 4, 7, 8, 10]

test = zipWithList (\x y -> (x, y)) testZipList1 testZipList2
