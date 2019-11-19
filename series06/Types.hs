-----------------------------------------------------------------
-- Aufgabe 1 - Werte
-----------------------------------------------------------------
{-
Geben Sie für die folgenden Typen jeweils 3 Werte an.
-}
{-
1. (Either Bool Int, String)
=> Wir haben ein Tupel
  linke Seite ein Either
      Either kann Left oder Right sein
        wobei Left ein Bool und rechts ein String ist
  rechte Seite ein String

data Either a b
  = Left a
  | Right b

-}
exps1_a :: (Either Bool Int, String)
exps1_a = (Left True, "test")

exps1_b = (Left True, "test2")

exps1_c = (Right 10, "test3")

{-
Prinzip des allgemeinsten Typens
:t gibt immer diesen
z.B a, b oder c
-}
id :: b -> b
id x = x

{-
konreter typ
  id :: int -> int
  id x = x
-}
--
{-
2. Either (Int, Bool) String
=> Either mit
  Left Tupel aus Int und Bool
  Rechts einen String
-}
exps2_a = Left (10, True)

exps2_b = Right "Test"

exps2_c = Left (12, False)

{-
3. Either (Maybe () ) ()

Maybe a
  = Just a
  | Nothing

=> Either mit
  Links ist ein Maybe der Nichts bekommt
    Entweder also ein Just Nichts
    oder ein Nothing
  Rechts ist Nichts
-}
exps3_a = Left (Just ())

exps3_b = Right ()

exps3_c = Left Nothing
