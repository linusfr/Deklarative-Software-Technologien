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
    deriving Show

-- Function
styleToAttr :: Style -> String
styleToAttr s = "stroke: " ++ show s ++ "; fill: " ++ show s ++ ";" 

-- Constant
defaultStyle :: Style
defaultStyle = Style Black

