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
styleToAttr (Style s) = "style=\'stroke:" ++ show s ++ "; fill:" ++ show s ++ "\'"

-- Constant
defaultStyle :: Style
defaultStyle = Style Black

-- formToSVG
formToSVG :: Form -> String

formToSVG (Rectangle (Point x1 y1) (Point x2 y2) (Style style)) =  "<rect x=\'" ++ show x1 ++ "\' y=\'" ++ show y1 ++ "\' width=\'" ++ 
    show x2 ++ "\' height=\'" ++ show y2 ++ "\' " ++ styleToAttr (Style style) ++ "/>"
formToSVG (Circle (Point x1 y1) radius (Style style)) =  "<circle cx=\'" ++ show (x1+1) ++ "\' cy=\'" ++ show (y1+1) ++ "\' r=\'" ++ 
    show radius ++ "\' " ++ styleToAttr (Style style) ++ "/>"

-- helper functions
    -- addition -> sums up two floats and returns them as a string
combinedTransition :: Float -> Float -> String
combinedTransition x1 x2 = show (x1+x2)

    -- doubling -> doubles a value and adds an offset (+2) for the default Stroke
doubling :: Float -> String
doubling radius = show (radius*2+2)

-- toSVG --> xmnls neccessary for the styles to be interpreted
toSVG :: Form -> String
toSVG (Rectangle (Point x1 y1) (Point x2 y2) (Style style)) = "<svg xmlns=\'http://www.w3.org/2000/svg\' width=\'" ++
    combinedTransition x1 x2 ++ "\' height=\'" ++ combinedTransition y1 y2 ++
    "\'>\n\t" ++ (formToSVG (Rectangle (Point x1 y1) (Point x2 y2) (Style style))) ++ "\n</svg>"
toSVG (Circle (Point x1 y1) radius (Style style)) = "<svg xmlns=\'http://www.w3.org/2000/svg\' width=\'" ++
    doubling radius ++ "\' height=\'" ++ doubling radius ++ "\'>\n\t" ++
    formToSVG (Circle (Point radius radius) radius (Style style)) ++ "\n</svg>"

-- generate Forms
    -- Rectangle
rectangle :: Float -> Float -> Form 
rectangle x y = (Rectangle (Point 0 0) (Point x y) (defaultStyle))
    -- Circle
circle :: Float -> Form 
circle radius = (Circle (Point 0 0) radius (defaultStyle))