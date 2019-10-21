module Graphic where

-- Graphic -> sum of forms
-- can be an empty Graphic or a Form with another Graphic
type Graphic = [Form]

-- transform one Form into a Graphic
single :: Form -> Graphic
single form = [form]

-- -- changes color of the graphic -> every form in graphic
colored :: Color -> Graphic -> Graphic
colored _ [] = []
colored c ((Rectangle p1 p2 _):fs) = Rectangle p1 p2 (Style c) : (colored c fs)
colored c ((Circle p f _):fs) = (Circle p f (Style c)) : (colored c fs)

-- -- takes two floats and a point and translates the point by the floats values
translatePoint :: Float -> Float -> Point -> Point
translatePoint x y (Point pX pY) = Point (pX + x) (pY + y)

translateForm :: Float -> Float -> Form -> Form
translateForm x y (Rectangle p1 p2 s) = Rectangle (translatePoint x y p1) p2 s
translateForm x y (Circle p f s)      = Circle (translatePoint x y p) f s

translate :: Float -> Float -> Graphic -> Graphic
translate _ _ []     = []
translate x y (f:fs) = (translateForm x y f) : (translate x y fs)

data BoundingBox
  = BoundingBoxNil
  | BoundingBox Point Point
  deriving (Show)

test :: [BoundingBox] 
test = [BoundingBox (Point 1 2) (Point 2 3), BoundingBox (Point 1 2) (Point 2 3), BoundingBox (Point 4 4) (Point 8 8)]

boundingBox :: Graphic -> BoundingBox
boundingBox [] = BoundingBoxNil
boundingBox ((Circle (Point pX pY) f _):fs)=
  unions ((BoundingBox (Point (pX - f) (pY - f)) (Point (pX + f) (pY + f))):(boundingBox fs):[]) 
boundingBox ((Rectangle (Point x1 y1) (Point x2 y2) _):fs) =
  unions ((BoundingBox (Point x1 y1) (Point (x1 + x2) (y1 + y2))):(boundingBox fs):[])

unions :: [BoundingBox] -> BoundingBox
unions [] = BoundingBoxNil
unions (b:bs) = union b (unions bs) 

-- -- combined two BoundingBox into one BoundingBox, included the size of both
union :: BoundingBox -> BoundingBox -> BoundingBox
union BoundingBoxNil b2 = b2
union b1 BoundingBoxNil = b1
union (BoundingBox (Point x1 y1) (Point x2 y2)) (BoundingBox (Point x3 y3) (Point x4 y4)) =
  BoundingBox
    (Point (min (min x1 x2) (min x3 x4)) (min (min y1 y2) (min y3 y4)))
    (Point (max (max x1 x2) (max x3 x4)) (max (max y1 y2) (max y3 y4)))

-- -- creates a new Graphic underneath each other
(===) :: Graphic -> Graphic -> Graphic
[] === (f:fs) = (f : fs)
(f:fs) === [] = (f : fs)
[] === [] = []
g1 === g2 =
  let BoundingBox (Point x1 y1) (Point x2 y2) = boundingBox g1
      BoundingBox (Point x3 y3) (Point x4 y4) = boundingBox g2
   in g1 ++
      translate ((x1 - x3) - ((x1 - x2) / 2) + ((x3 - x4) / 2)) (y2 - y3) g2

-- -- center two Graphics over each other
atop :: Graphic -> Graphic -> Graphic
atop (f:fs) (j:js) =
  let BoundingBox (Point x1 y1) (Point x2 y2) = boundingBox (f : fs)
      BoundingBox (Point x3 y3) (Point x4 y4) = boundingBox (j : js)
   in (f : fs) ++
      (translate
         ((x1 - x3) - ((x1 - x2) / 2) + ((x3 - x4) / 2))
         ((y1 - y3) - ((y1 - y2) / 2) - ((y4 - y3) / 2))
         (j : js))
atop _ _ = []

-- ----------------------------------------------------------------------------------------------------------------
-- -- Graphics --> from series01 adapted for series02
-- Point
data Point =
  Point Float Float
  deriving (Show)

-- -- Forms
data Form
  = Rectangle Point Point Style
  | Circle Point Float Style
  deriving (Show)

data Color
  = Black
  | Red
  | Green
  | Blue
  | Yellow
  deriving (Show)

toLower :: Color -> String
toLower Black  = "black"
toLower Red    = "red"
toLower Green  = "green"
toLower Blue   = "blue"
toLower Yellow = "yellow"

-- Style
data Style =
  Style Color
  deriving (Show)

styleToAttr :: Style -> String
styleToAttr (Style s) =
  "style=\"stroke:" ++ toLower s ++ "; fill:" ++ toLower s ++ "\""

-- Constant
defaultStyle :: Style
defaultStyle = Style Black

-- formToSVG
formToSVG :: Form -> String
formToSVG (Rectangle (Point x1 y1) (Point x2 y2) style) =
  "<rect x=\"" ++
  show x1 ++
  "\" y=\"" ++
  show y1 ++
  "\" width=\"" ++
  show x2 ++ "\" height=\"" ++ show y2 ++ "\" " ++ styleToAttr style ++ "/>"
formToSVG (Circle (Point x1 y1) radius style) =
  "<circle cx=\"" ++
  show (x1 + 1) ++
  "\" cy=\"" ++
  show (y1 + 1) ++
  "\" r=\"" ++ show radius ++ "\" " ++ styleToAttr style ++ "/>"

-- toSVG --> create the xml-code for all Forms in Graphic
toSVG :: Graphic -> String
toSVG (f:fs) = (formToSVG f) ++ (toSVG fs)
toSVG []     = ""

-- svgBorder --> creates the svg-Label around the xml-code, xmnls neccessary for the styles to be interpreted
svgBorder :: String -> String
svgBorder allGraphics =
  "<svg xmlns=\"http://www.w3.org/2000/svg\">\n\t" ++ allGraphics ++ "\n</svg>"

rectangle :: Float -> Float -> Graphic
rectangle width height =
  single (Rectangle (Point 0 0) (Point width height) defaultStyle)

circle :: Float -> Graphic
circle radius = single (Circle (Point radius radius) radius defaultStyle)
