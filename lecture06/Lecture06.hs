--------------------------------------------------------------
-- vorlesung 5 (code baut drauf auf)
--------------------------------------------------------------
--
-- Linsen
-- view/update Problem
-- Getter/Setter
-- Record Syntax
--
data Point =
  Point
    { coordX :: Int
    , coordY :: Int
    } -- coordX :: Point -> Int

setCoordX :: Point -> Int -> Point
setCoordX p x = p {coordX = x}

-- setCoordX (Point _ y) x = Point x y
setCoordY :: Point -> Int -> Point
setCoordY p y = p {coordX = y}

-- setCoordX (Point _ y) x = Point x y
data Player
  = Mario
      { lives    :: Int
      , position :: Point
      }
  | Yoshi
      { lives    :: Int
      , position :: Point
      , carrying :: Enemy
      }
  deriving (Show)

data World =
  World
    { player :: Player
    }
  deriving (Show)

setPosition :: Player -> Point -> Player
setPosition p coord = p {position = coord}

setPlayer :: World -> Player -> World
setPlayer w p = w {player = p}

mario :: Player
mario = Mario 5 (Point 0 0)

-- mario = Mario {lives = 5, position = Point 0 0}
--
world :: World
world = World {player = mario}

playerX :: World -> Int
playerX w = coordX (postition (player w))

playerX = coordX . position . player

--------------------------------------------------------------
-- anfang vorlesung 6
--------------------------------------------------------------
--
setPlayerX w x = setPosition (player w) (setCoordX (position (player w)) x)

--
-- Linsen (lenses)
data Lens s v =
  Lens
    { getterL :: s -> v
    , setterL :: s -> v -> s
    }

-- source
-- view
playerLens :: Lens World Player
playerLens = Lens player setPlayer

positionLens :: Lens Player Position
positionLens = Lens position setPosition

coordXLens :: Lens Point Int
coordXLens = Lens coordX setCoordX

playerX2 :: World -> Int
playerX2 = getterL coordXLens . getterL positionLens . getterL playerLens

setPlayerX2 :: World -> Int -> World
setPlayerX2 w x =
  setPlayer
    w
    (setterL
       positionLens
       (player w)
       (setterL coordXlens (position (player w)) x))

(|.|) :: Lens b -> Lens a b -> Lens a c
Lens getBC setBC |.| Lens getAB setAB = Lens getAC setAC
  where
    getAC = getBC . getAB
    setAC sA vC = setAB sA (setBC (getAB sA) vC)

playerXLens :: Lens World Int
playerXLens = coordXLens |.| positionLens |.| playerLens

setPlayerX3 :: World -> Int -> World
setPlayerX3 = setterL playerXLens

coordXLensBad :: Lens Point Int
coordXLensBad = Lens coordX setCoordX
  where
    setCoordX p x = p {coordX = x, coordY = x}

{-
-- get-put
forall s. put s (get s) == s

s :: Point
s = Point 1 2

put s (get s)

setCoordX s (coordXs)

setCoordX s 1

Point 1 1
=/=
s
-}
coordXLens :: Lens Point Int
coordXLens = Lens coordX setCoordX

setCoordX :: Point -> Int -> Point
setCoordX p x = p {coordX = x}

setCoordY :: Point -> Int -> Point
setCoordY p y = p {coordY = y}
{-
-- put-get
forall s. forall v. get (put s v) == v

-- put-get & get-put
well-behaved

-- put-put

very well-behaved
-}
