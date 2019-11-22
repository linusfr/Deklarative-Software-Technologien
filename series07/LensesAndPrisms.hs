module LensesAndPrisms where

{-
Definieren Sie eine Linse für die Kopfkomponente einer Liste headLens :: Lens [a ] a. Welche der drei
Linsengesetze erfüllt Ihre Implementierung und welche nicht?
• Definieren Sie ein Prisma für die Kopfkomponente einer Liste headPrism :: Prism [a ] a.
• Definieren Sie eine Funktion nthPrism :: Int → Prism [a ] a. Diese Funktion erhält einen Index und liefert
  ein Prisma, das an diesen Index in einer Liste projiziert bzw. diesen Index in einer Liste aktualisiert.
-}
-- Linsen (lenses)
data Lens s v =
  Lens
    { getterL :: s -> v
    , setterL :: s -> v -> s
    }

headLens :: Lens [a] a
headLens = Lens getHead setHead
  where
    getHead :: [a] -> a
    getHead []     = error "empty List"
    getHead (a:as) = a
    setHead :: [a] -> a -> [a]
    setHead [] v     = [v]
    setHead (a:as) v = v : as

{-
get
    get:: Source -> View
put
    put :: Source -> View -> Source

get-put
    Das erste Gesetz besagt, dass wir keine Veränderung observieren, wenn wir eine
    Komponente durch das Ergebnis ersetzen, das wir zuvor aus der Struktur geholt haben.
    Formal kann dieses Gesetz wie folgt ausgedrückt werden.

    für alle s vom Typen Source gilt, dass:
        put s (get s) = s
-}
listRules = [1 .. 5]

getPutRule =
  ((setterL headLens) listRules ((getterL headLens) listRules)) == listRules

{-
put-get
    Das zweite Gesetz besagt, dass wir mit Hilfe des Getter s den Wert zurückerhalten
    möchten, den wir zuvor mit Hilfe eines Setter s eingesetzt haben.

    für alle s :: Source, v :: View gilt, dass
        get (put s v) = v
-}
value = 5

putGetRule = getterL headLens ((setterL headLens) listRules value) == value

{-
put-put
    Das dritte Gesetz besagt, dass Setter immer vorherige Aufrufe von diesem überschreiben.
    "put überschreibt die Aktion eines vorherigen puts"

    für alle s :: Source, v :: View, v' :: View gilt, dass
        put (put s v ) v' = put s v'
-}
value' = 10

putPutRule =
  (setterL headLens) ((setterL headLens) listRules value) value' ==
  (setterL headLens) listRules value'

{-
Definieren Sie ein Prisma für die Kopfkomponente einer Liste headPrism :: Prism [a ] a.
-}
data Prism s v =
  Prism
    { getterP :: s -> Maybe v
    , setterP :: s -> v -> s
    }

headPrism :: Prism [a] a
headPrism = Prism getHead setHead
  where
    getHead :: [a] -> Maybe a
    getHead []     = Nothing
    getHead (a:as) = Just a
    setHead :: [a] -> a -> [a]
    setHead [] v     = [v]
    setHead (a:as) v = v : as

{-
Definieren Sie eine Funktion nthPrism :: Int → Prism [a] a. Diese Funktion erhält einen Index und liefert
ein Prisma, das an diesen Index in einer Liste projiziert bzw. diesen Index in einer Liste aktualisiert.
-}
nthPrism :: Int -> Prism [a] a
nthPrism i = Prism (getGivenIndex i) (setGivenIndex i)
  where
    getGivenIndex :: Int -> [a] -> Maybe a
    getGivenIndex _ [] = Nothing
    getGivenIndex i (a:as) =
      if i > 0
        then getGivenIndex (i - 1) as
        else Just a
    setGivenIndex :: Int -> [a] -> a -> [a]
    setGivenIndex _ [] v = [v]
    setGivenIndex i (a:as) v =
      if i > 0
        then a : (setGivenIndex (i - 1) as v)
        else v : as

nthPrism' :: Int -> Prism [a] a
nthPrism' i = Prism (getGivenIndex i) (setGivenIndex i)
  where
    getGivenIndex :: Int -> [a] -> Maybe a
    getGivenIndex _ [] = Nothing
    getGivenIndex i (a:as) =
      if i > 0
        then getGivenIndex (i - 1) as
        else (getterP headPrism) (a : as)
    setGivenIndex :: Int -> [a] -> a -> [a]
    setGivenIndex _ [] v = [v]
    setGivenIndex i (a:as) v =
      if i > 0
        then a : (setGivenIndex (i - 1) as v)
        else (setterP headPrism) (a : as) v

{-
Aufgabe 2 - Prismen komponieren
In dieser Aufgabe sollen Sie Funktionen zur Arbeit mit Prismen definieren.

• Definieren Sie eine Funktion lift :: Lens s a → Prism s a, mit deren Hilfe man aus einer Linse ein Prisma
machen kann.
-}
-----------------------------------------
-- how do we pattern match for an error?
-- if getterL can't get a result
-----------------------------------------
lift :: Lens s v -> Prism s v
lift lens = Prism (getterP (getterL lens)) (setterL lens)
  where
    getterP :: (s -> v) -> s -> Maybe v
    getterP f s = Just (f s)

{-
• Definieren Sie die Funktion (|..|) :: Prism b c → Prism a b → Prism a c, die genutzt werden kann, um zwei
Prismen zu komponieren.
-}
-- (|..|) :: Prism b c -> Prism a b -> Prism a c
-- Prism getBC setBC |..| Prism getAB setAB = Prism getAC setAC
--   where
--     getAC =
--       let maybe = getAB
--        in if maybe == Just v
--             then getBC . stripMaybe maybe
--             else Nothing
-- stripMaybe :: Maybe v -> v
-- stripMaybe (Just v) = v
-- (|.|) :: Lens b c -> Lens a b -> Lens a c
-- Lens getBC setBC |.| Lens getAB setAB = Lens getAC setAC
--   where
--     getAC = getBC . getAB
--     setAC sA vC = setAB sA (setBC (getAB sA) vC)
{-
Aufgabe 3 - Faltungen auf Listen
  In dieser Aufgabe sollen Sie sich mit den Funktionen foldr und foldl beschäftigen. Definieren Sie die folgenden
  Funktionen mit foldr oder mit foldl. Überlegen Sie sich jeweils, welche der Funktionen sich zur Definition besser
  eignet.

  • Die Funktion orList :: [Bool ] → Bool ist die Verallgemeinerung von (||) auf eine Liste von booleschen
    Werten.
-}
orList :: [Bool] -> Bool
orList = foldr (||) False
