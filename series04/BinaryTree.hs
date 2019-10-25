module BinaryTree where

data BinTree v
  = Node (BinTree v) (BinTree v)
  | Leaf v
  deriving (Show)

-- sumTree -> sum of the values of each leaf of a tree
sumTree :: BinTree Int -> Int
sumTree (Node b1 b2) = sumTree b1 + sumTree b2
sumTree (Leaf v)     = v

square :: Int -> Int
square x = x * 2

mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Node b1 b2) = Node (mapTree f b1) (mapTree f b2)
mapTree f (Leaf v)     = Leaf (f v)

testTree :: BinTree Int
testTree = Node (Node (Leaf 10) (Leaf 20)) (Node (Leaf 30) (Leaf 40))

sumOfTree = sumTree testTree

mapOfTree = sumTree (mapTree square testTree)
