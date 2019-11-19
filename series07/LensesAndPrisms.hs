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