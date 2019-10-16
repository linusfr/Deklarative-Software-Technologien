import           List

list :: [Int]
list = 1 : [2]

list2 :: [Int]
list2 = 1 : [3, 4, 5]

test :: [Int]
test = appendList list list2

string :: String
string = "H"

string2 :: String
string2 = "Hallo"

test2 :: Bool
test2 = isPrefixOfString string string2

test3 :: String
test3 = reverseList string2

test4 :: [Int]
test4 = 1 : 2 : [3]
