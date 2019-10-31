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

temp :: World -> Point
temp w = position (player w)
temp w = (position . player) w

temp = position . player

-- f (g a)
-- position . player
--
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \a -> f (g a)

--
inc :: Int -> Int
inc x = x + 1

square :: Int -> Int
square x = x * x
--
-- f . g = \a -> f (g a)
-- inc . square == \a -> inc (square a)
