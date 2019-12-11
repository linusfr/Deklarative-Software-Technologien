module ExtendExpr where

{-
Aufgabe 2 - Erweiterung der Ausdrücke

  In dieser Aufgabe sollen Sie die Implementierung der Ausdrücke um zusätzliche Funktionalitäten erweitern.

  • Fügen Sie die Konstruktoren Sub und Div zum Datentyp BinOp hinzu. Der Konstruktor Div beschreibt
    dabei die ganzzahlige Division.
-}
data BinOp
  = Add
  | Sub
  | Div
  | Mul
  deriving (Show)

{-
  • Passen Sie die Funktion eval so an, dass sie den Typ Expr → Maybe Int erhält.
  Falls einer der rekursiven Aufrufe Nothing liefert, soll die Funktion ebenfalls Nothing liefern.
  Ändern Sie die Funktion so ab, dass sie den Wert Nothing liefert, falls durch Null geteilt wird.
-}
data Expr
  = BinApp BinOp Expr Expr
  | Num Int
  deriving (Show)

eval :: Expr -> Maybe Int
eval (BinApp Add ex1 ex2) = ((eval ex1) + (eval ex2))
eval (BinApp Sub ex1 ex2) = ((eval ex1) - (eval ex2))
eval (BinApp Mul ex1 ex2) = ((eval ex1) * (eval ex2))
eval (BinApp Div ex1 ex2) =
  if (eval ex2) == 0
    then Nothing
    else (div (eval ex1) (eval ex2))
eval (Num x) = Just x
{-
  • Nutzen Sie die Faltung für den Datentyp Expr, um eine Funktion eval0 :: Expr → Maybe Int zu definieren,
    die sich genau so verhält, wie die zuvor definierte Funktion.
-}
{-
    Definieren Sie außerdem eine Funktion eval :: Expr → Int, die einen Ausdruck auswertet.
-}
