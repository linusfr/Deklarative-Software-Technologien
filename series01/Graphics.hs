-- Point
data Point = Point Float Float
-- point :: Point
-- point = Point 3.4 4.5

-- Forms
data Form = 
    Rectangle Point Point Style
    | Circle Point Float Style

-- Colors
data Color = 
    Black
    | Red
    | Green
    | Blue
    | Yellow
    deriving Show

-- Style
data Style = Style Color

-- Function
styleToAttr :: Style -> String
