import           Graphic

-- a) ++ Check
testA :: Graphic
testA = [(Rectangle (Point 10 10) (Point 200 200) defaultStyle),(Rectangle (Point 10 10) (Point 200 200) defaultStyle),(Rectangle (Point 400 500) (Point 700 200) defaultStyle)]

testB :: Graphic
testB = [(Rectangle (Point 10 10) (Point 200 200) defaultStyle),(Rectangle (Point 10 10) (Point 1000 1000) defaultStyle)]

listA :: [Int]
listA = [1,2,3,4]

addTwo :: Int -> Int
addTwo x = x+2

isGreaterTwo :: Int -> Bool
isGreaterTwo x = x>2

newList :: (Int -> Int) -> [Int] -> [Int]
newList f [] = []
newList f (x:xs) = f x : newList f xs

newList2 :: (Int -> Bool) -> [Int] -> [Bool]
newList2 f [] = []
newList2 f (x:xs) = f x : newList2 f xs

formA = rectangle 10 20
recolored = recolor colored Red (formA) 

test = toSVG formToSVG recolored