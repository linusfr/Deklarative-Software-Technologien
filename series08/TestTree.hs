import           Tree

tree = Node (Node Empty 20 Empty) 10 (Node Empty 20 Empty)

---------------------------------------------
-- sumTree
---------------------------------------------
treeSumTest = sumTree tree

---------------------------------------------
-- values
---------------------------------------------
treeListTest = values tree

---------------------------------------------
-- mapTree
---------------------------------------------
treeMapTest = mapTree (\x -> x + 1) tree
