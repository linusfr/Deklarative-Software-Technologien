import           Graphic

-- a) ++ Check
testA :: Graphic
testA = [(Rectangle (Point 10 10) (Point 200 200) defaultStyle),(Rectangle (Point 10 10) (Point 200 200) defaultStyle),(Rectangle (Point 400 500) (Point 700 200) defaultStyle)]

testB :: Graphic
testB = [(Rectangle (Point 10 10) (Point 200 200) defaultStyle),(Rectangle (Point 10 10) (Point 1000 1000) defaultStyle)]
