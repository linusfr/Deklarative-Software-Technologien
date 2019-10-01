-- Point
data Point = Point Float Float
     deriving Show
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

-- formToSVG
formToSVG :: Form -> String

    -- Rect
        -- <svg width="400" height="180">
        --  <rect x="50" y="20" width="150" height="150"
        --  style="fill:blue;stroke:pink;stroke-width:5;fill-opacity:0.1;stroke-opacity:0.9" />
        -- </svg>
        -- The x attribute defines the left position of the rectangle (e.g. x="50" places the rectangle 50 px from the left margin)
        -- The y attribute defines the top position of the rectangle (e.g. y="20" places the rectangle 20 px from the top margin)
        -- The CSS fill-opacity property defines the opacity of the fill color (legal range: 0 to 1)
        -- The CSS stroke-opacity property defines the opacity of the stroke color (legal range: 0 to 1)
    -- Circle
        -- <svg height="100" width="100">
        -- <circle cx="50" cy="50" r="40" stroke="black" stroke-width="3" fill="red" />
        -- </svg>
        -- The cx and cy attributes define the x and y coordinates of the center of the circle. If cx and cy are omitted, the circle's center is set to (0,0)
        -- The r attribute defines the radius of the circle
    
formToSVG (Rectangle (Point x1 y1) (Point x2 y2) style) =  "<rect x=" ++ show x1 ++ " y=" ++ show y1 ++ " width=" ++ show x2 ++ " height=" ++ show y2 ++ " style=" ++ show style ++ " />"
formToSVG (Circle (Point x1 y1) radius style) =  "<circle cx= " ++ show x1 ++ " cy=" ++ show y1 ++ "r=" ++ show radius ++ " style=" ++ show style ++ " />"

rectTest = Rectangle (Point 1.0 2.0) (Point 3.0 4.0) (Style Green)
rectOutput = formToSVG rectTest
circleTest = Circle (Point 3.0 4.0) 5.0 (Style Red)
circleOutput = formToSVG circleTest

-- toSVG
toSVG :: Form -> String
toSVG x = "<svg>" ++ (formToSVG x) ++ " </svg>"

output = toSVG rectTest
