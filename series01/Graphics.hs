module Graphics where
    
-- Point
data Point = Point Float Float
     deriving Show

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
styleToAttr (Style s) = "stroke=" ++ show s ++ " fill=" ++ show s ++ " "

-- Constant
defaultStyle :: Style
defaultStyle = Style Black

-- formToSVG
formToSVG :: Form -> String

formToSVG (Rectangle (Point x1 y1) (Point x2 y2) (Style style)) =  "<rect x=" ++ show x1 ++ " y=" ++ show y1 ++ " width=" ++ show x2 ++ " height=" ++ show y2 ++ " " ++ styleToAttr (Style style) ++ " />"
formToSVG (Circle (Point x1 y1) radius (Style style)) =  "<circle cx=" ++ show (x1+1) ++ " cy=" ++ show (y1+1) ++ " r=" ++ show radius ++ " " ++ styleToAttr (Style style) ++ " />"

-- helper functions
    -- addition
combinedTransition :: Float -> Float -> String
combinedTransition x1 x2 = show (x1+x2)

    -- doubling
doubling :: Float -> String
doubling radius = show (radius*2+2)

-- toSVG
toSVG :: Form -> String
toSVG (Rectangle (Point x1 y1) (Point x2 y2) (Style style)) = "<svg width=" ++ combinedTransition x1 x2 ++ " height=" ++ combinedTransition y1 y2 ++ "> " ++ (formToSVG (Rectangle (Point x1 y1) (Point x2 y2) (Style style))) ++ " </svg>"
toSVG (Circle (Point x1 y1) radius (Style style)) = "<svg width="  ++ doubling radius ++ " height=" ++ doubling radius ++ "> " ++ formToSVG (Circle (Point radius radius) radius (Style style)) ++ " </svg>"

-- generate Graphics
rectangle :: Float -> Float -> Form 
rectangle x y = (Rectangle (Point 0 0) (Point x y) (defaultStyle))

circle :: Float -> Form 
circle radius = (Circle (Point 0 0) radius (defaultStyle))