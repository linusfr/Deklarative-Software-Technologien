module Style where

-----------------------------------------------------------------
-- Aufgabe 2 - Graphistile
-----------------------------------------------------------------
{-
Ändern Sie die Implementierung des Style so ab,
dass es neben der bisherigen
Color noch einen Stroke gibt.
Die Color definiert die Füllfarbe der Graphik.
Der Stroke besteht aus einer Farbe und einer Linienstärke
und beschreibt die Linie der Graphik.

style = "fill:black; stroke:yellow; stroke-width:30"

-}
-- Graphic -> sum of forms
-- can be an empty Graphic or a Form with another Graphic
type Graphic = [Form]

-- transform one Form into a Graphic
single :: Form -> Graphic
single form = [form]

-- -- changes color of the graphic -> every form in graphic
--    changed to a map Function
colored :: Color -> Form -> Form
colored c (Rectangle p1 p2 _) = Rectangle p1 p2 (Style c (Stroke c 1))
colored c ((Circle p f _))    = (Circle p f (Style c (Stroke c 1)))

recolor :: (Color -> Form -> Form) -> Color -> Graphic -> Graphic
recolor _ _ []               = []
recolor colorIt color (f:fs) = map (colorIt color) (f : fs)

--  colorIt color f : (recolor colorIt  color fs)
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

boundingBox :: Graphic -> BoundingBox
boundingBox [] = BoundingBoxNil
boundingBox ((Circle (Point pX pY) f _):fs) =
  unions
    ((BoundingBox (Point (pX - f) (pY - f)) (Point (pX + f) (pY + f))) :
     (boundingBox fs) : [])
boundingBox ((Rectangle (Point x1 y1) (Point x2 y2) _):fs) =
  unions
    ((BoundingBox (Point x1 y1) (Point (x1 + x2) (y1 + y2))) :
     (boundingBox fs) : [])

unions :: [BoundingBox] -> BoundingBox
unions []     = BoundingBoxNil
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
toSVG :: (Form -> String) -> Graphic -> String
toSVG fToSVG (f:fs) = (fToSVG f) ++ (toSVG fToSVG fs)
toSVG _ []          = ""

-- svgBorder --> creates the svg-Label around the xml-code, xmnls neccessary for the styles to be interpreted
svgBorder :: String -> String
svgBorder allGraphics =
  "<svg xmlns=\"http://www.w3.org/2000/svg\">\n\t" ++ allGraphics ++ "\n</svg>"

rectangle :: Float -> Float -> Graphic
rectangle width height =
  single (Rectangle (Point 0 0) (Point width height) defaultStyle)

circle :: Float -> Graphic
circle radius = single (Circle (Point radius radius) radius defaultStyle)

-- Point
data Point =
  Point Float Float
  deriving (Show)

data Color
  = Black
  | Red
  | Green
  | Blue
  | Yellow
  | Gray
  deriving (Show)

toLower :: Color -> String
toLower Black  = "black"
toLower Red    = "red"
toLower Green  = "green"
toLower Blue   = "blue"
toLower Yellow = "yellow"
toLower Gray   = "gray"

---------------------------------------------------------------------------------------
-- STROKE
---------------------------------------------------------------------------------------
data Stroke =
  Stroke
    { strokeColor :: Color
    , strokeWidth :: Float
    }
  deriving (Show)

-- Style
data Style =
  Style
    { fillColor :: Color
    , stroke    :: Stroke
    }
  deriving (Show)

styleToAttr :: Style -> String
styleToAttr (Style fillColor (Stroke strokeColor strokeWidth)) =
  "style=\"stroke:" ++
  toLower strokeColor ++
  "; fill:" ++
  toLower fillColor ++ "; stroke-width:" ++ show strokeWidth ++ "\""

-- Constant
defaultStyle :: Style
defaultStyle = Style Black (Stroke Black 1)

---------------------------------------------------------------------------------------
-- LENS
---------------------------------------------------------------------------------------
{-
Definieren Sie die folgenden Linsen unter Verwendung von (|.|).
    – fillColorStyleLens :: Lens Style Color
    – strokeColorStyleLens :: Lens Style Color
    – strokeWidthStyleLens :: Lens Style Float

      Form {
          Style{
            Color
          }
      }
-}
-- Linsen (lenses)
data Lens s v =
  Lens
    { getterL :: s -> v
    , setterL :: s -> v -> s
    }

(|.|) :: Lens b c -> Lens a b -> Lens a c
Lens getBC setBC |.| Lens getAB setAB = Lens getAC setAC
  where
    getAC = getBC . getAB
    setAC sA vC = setAB sA (setBC (getAB sA) vC)

---------------------------------------------------------------------------------------
-- fillColorStyleLens
---------------------------------------------------------------------------------------
{-
fillColorStyleLens :: Lens Style Color
  1. Style muss ein Record sein für den automatischen Getter
  2. Lens von Style nach Color   (a -> b)
-}
-- better!
fillColorStyleLens :: Lens Style Color
fillColorStyleLens = Lens fillColor setColor
  where
    setColor :: Style -> Color -> Style
    setColor s c = s {fillColor = c}

-- setColor :: Style -> Color -> Style
-- setColor s c = s {fillColor = c}
-- getStyleColor :: Style -> Color
-- getStyleColor = getterL fillColorStyleLens
-- setStyleColor :: Style -> Color -> Style
-- setStyleColor = setterL fillColorStyleLens
---------------------------------------------------------------------------------------
-- strokeColorStyleLens
---------------------------------------------------------------------------------------
{-
strokeColorStyleLens :: Lens Style Color
  1. Stroke muss ein Record sein für automatischen Getter
  2. Lens von Style zu Stroke (a -> b)
  3. Lens von Stroke nach StrokeColor  (b -> c)
  4. Lenskombination (|.|) von Style nach StrokeColor
    (a -> b -> c)
      2. dann 3.
-}
setStrokeStyle :: Style -> Stroke -> Style
setStrokeStyle style stroke = style {stroke = stroke}

strokeStyleLens :: Lens Style Stroke
strokeStyleLens = Lens stroke setStrokeStyle

setStrokeColor :: Stroke -> Color -> Stroke
setStrokeColor s c = s {strokeColor = c}

strokefillColorStyleLens :: Lens Stroke Color
strokefillColorStyleLens = Lens strokeColor setStrokeColor

strokeColorStyleLens :: Lens Style Color
strokeColorStyleLens = strokefillColorStyleLens |.| strokeStyleLens

getStyleStrokeColor :: Style -> Color
getStyleStrokeColor = getterL strokeColorStyleLens

setStyleStrokeColor :: Style -> Color -> Style
setStyleStrokeColor = setterL strokeColorStyleLens

---------------------------------------------------------------------------------------
-- strokeWidthStyleLens
---------------------------------------------------------------------------------------
{-
strokeWidthStyleLens :: Lens Style Float
  1. Stroke muss ein Record sein für automatischen Getter
  2. Lens von Style zu Stroke (a -> b)
  3. Lens von Stroke nach StrokeWidth  (b -> c)
  4. Lenskombination (|.|) von Style nach StrokeWidth
    (a -> b -> c)
      2. dann 3.
-}
setStrokeWidth :: Stroke -> Float -> Stroke
setStrokeWidth s f = s {strokeWidth = f}

strokeWidthLens :: Lens Stroke Float
strokeWidthLens = Lens strokeWidth setStrokeWidth

strokeWidthStyleLens :: Lens Style Float
strokeWidthStyleLens = strokeWidthLens |.| strokeStyleLens

getStyleStrokeWidth :: Style -> Float
getStyleStrokeWidth = getterL strokeWidthStyleLens

setStyleStrokeWidth :: Style -> Float -> Style
setStyleStrokeWidth = setterL strokeWidthStyleLens

{-
In dieser Aufgabe sollen Sie die Stile aus der vorherigen Aufgabe in der Graphik-Bibliothek verwenden.
• Definieren Sie die folgenden Linsen unter Verwendung von (|.|).
  – fillColorFormLens :: Lens Form Color
  – strokeColorFormLens :: Lens Form Color
  – strokeWidthFormLens :: Lens Form Float
-}
---------------------------------------------------------------------------------------
-- fillColorFormLens
---------------------------------------------------------------------------------------
{-
fillColorFormLens :: Lens Form Color
  1. Form und Color müssen Records sein für automatische Getter
  2. Lens von Form nach Style   (a -> b)
  3. Lens von Style nach Color  (b -> c)
  4. Lenskombination (|.|) von Form nach Color
    (a -> b -> c)
      2. dann 3.
-}
data Form
  = Rectangle
      { positionPoint :: Point
      , sizePoint     :: Point
      , style         :: Style
      }
  | Circle
      { positionPoint :: Point
      , radius        :: Float
      , style         :: Style
      }
  deriving (Show)

setStyle :: Form -> Style -> Form
setStyle f s = f {style = s}

styleLens :: Lens Form Style
styleLens = Lens style setStyle

fillColorFormLens :: Lens Form Color
fillColorFormLens = fillColorStyleLens |.| styleLens

getFormColor :: Form -> Color
getFormColor = getterL fillColorFormLens

setFormColor :: Form -> Color -> Form
setFormColor = setterL fillColorFormLens

---------------------------------------------------------------------------------------
-- strokeColorFormLens
---------------------------------------------------------------------------------------
{-
strokeColorFormLens :: Lens Form Color
  1. Form und Style müssen Records sein für automatische Getter
  2. Lens von Form nach Style   (a -> b)
      => StyleLens
  3. Lens von Style nach strokeColor  (b -> d)
      => strokeColorStyleLens
  4. Lenskombination (|.|) von Form nach strokeColor
    (a -> b -> d)
      2. dann 3.
-}
strokeColorFormLens :: Lens Form Color
strokeColorFormLens = strokeColorStyleLens |.| styleLens

getFormStrokeColor :: Form -> Color
getFormStrokeColor = getterL strokeColorFormLens

setFormStrokeColor :: Form -> Color -> Form
setFormStrokeColor = setterL strokeColorFormLens

---------------------------------------------------------------------------------------
-- strokeWidthFormLens
---------------------------------------------------------------------------------------
{-
strokeWidthFormLens :: Lens Form Float
  1. Form und Style müssen Records sein für automatische Getter
  2. Lens von Form nach Style   (a -> b)
      => StyleLens
  3. Lens von Style nach strokeColor  (b -> d)
      => strokeWidthStyleLens
  4. Lenskombination (|.|) von Form nach strokeWidth
    (a -> b -> d)
      2. dann 3.
-}
strokeWidthFormLens :: Lens Form Float
strokeWidthFormLens = strokeWidthStyleLens |.| styleLens

getFormStrokeWidth :: Form -> Float
getFormStrokeWidth = getterL strokeWidthFormLens

setFormStrokeWidth :: Form -> Float -> Form
setFormStrokeWidth = setterL strokeWidthFormLens

fc :: Color -> Graphic -> Graphic
fc _ []     = []
fc c (f:fs) = setFormColor f c : fc c fs

sc :: Color -> Graphic -> Graphic
sc _ []     = []
sc c (f:fs) = setFormStrokeColor f c : sc c fs

sw :: Float -> Graphic -> Graphic
sw _ []      = []
sw fl (f:fs) = setFormStrokeWidth f fl : sw fl fs

{-
Definieren Sie einen Operator (|>), der es erlaubt, auf die folgende Weise ein Licht für die Ampel zu
definieren.
    - light :: Color → Graphic
    - light c = atop (circle 50 |> fc c |> sc Gray |> sw 5) (rectangle 120 120)
-}
light :: Color -> Graphic
light c = atop (rectangle 120 120) (circle 50 |> fc c |> sc Gray |> sw 5)

{-
kein Pattern Matching
  => sind nicht spezifisch mit dem typen
  => einfach generisch definieren
-}
-- (|>) :: Graphic -> (Graphic -> Graphic) -> Graphic
-- g |> f = f g
(|>) :: a -> (a -> a) -> a
g |> f = f g

-- am besten
fc :: Color -> Graphic -> Graphic
fc = help fillColorFormLens

help :: Lens b a -> a -> [b] -> [b]
help lens c = map (\f -> setterL lens f c)
  -- well-behaved
  -- very well behaved

--
-- for all s. forall v. get (put s v) == v
-- put-get % get-put
-- put-put
{-
OBACHT -> Record Syntax
-}
data Player
  = Mario
      { lives    :: int
      , position :: Point
      }
  | Yoshi
      { lives    :: int
      , position :: Point
      , enemy    :: Player
      }

-- syntax schenkt einem funktion
-- lives :: Player -> Int
data Prism s v =
  Prism
    { getterP :: s -> Maybe v
    , setterP :: s -> v -> s
    }

-- durch das Maybe sieht man Laufzeitfehler schon beim Compilieren
enemy :: Player -> Maybe Enemy
enemy (Mario _ _)   = Nothing
enemy (Yoshi _ _ e) = Just e

setEnemy :: Player -> Enemy -> Player
setEnemy (Mario l p) e   = Mario l p
setEnemy (Yoshi l p _) e = Yoshi l p e
