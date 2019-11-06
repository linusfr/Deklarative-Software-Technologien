-----------------------------------------------------------------
-- Aufgabe 1 - Werte
-----------------------------------------------------------------
{-
Geben Sie für die folgenden Typen jeweils 3 Werte an.
-}
{-
1. (Either Bool Int, String)

data Either a b
  = Left a
  | Right b

-}
exps1 = (Left True, "test")

{-
2. Either (Int Bool), String
-}
exps2 = Left (1, True)

{-
3. Either (Maybe () ) ()
-}
exps3 = Left (Just ())
