module ExtendExpr where

{-
Aufgabe 2 - Erweiterung der Ausdrücke
In dieser Aufgabe sollen Sie die Implementierung der Ausdrücke um zusätzliche Funktionalitäten erweitern.
• Fügen Sie die Konstruktoren Sub und Div zum Datentyp BinOp hinzu. Der Konstruktor Div beschreibt
dabei die ganzzahlige Division.

• Passen Sie die Funktion eval so an, dass sie den Typ Expr → Maybe Int erhält. Falls einer der rekursiven
Aufrufe Nothing liefert, soll die Funktion ebenfalls Nothing liefern. Ändern Sie die Funktion so ab, dass sie
den Wert Nothing liefert, falls durch Null geteilt wird.

• Nutzen Sie die Faltung für den Datentyp Expr, um eine Funktion eval0 :: Expr → Maybe Int zu definieren,
die sich genau so verhält, wie die zuvor definierte Funktion.
-}
data BinOp
  = Add
  | Sub
  | Div
  | Mul
  deriving (Show)

data Expr
  = BinApp BinOp Expr Expr
  | Num Int
  deriving (Show)

showExpr :: Expr -> String
showExpr (BinApp op ex1 ex2) =
  "(" ++ showExpr ex1 ++ display op ++ showExpr ex2 ++ ")"
  where
    display Add = " + "
    display Sub = " - "
    display Mul = " * "
    display Div = " / "
showExpr (Num x) = show x
{-
    Definieren Sie außerdem eine Funktion eval :: Expr → Int, die einen Ausdruck auswertet. Die Funktion eval liefert
    für den Ausdruck testExpr zum Beispiel das Resultat 10.
-}
-- testInfinite :: a -> Maybe Int
-- testInfinite x =
--   if (isInt x)
--     then Just x
--     else Nothing
-- eval :: Expr -> Maybe Int
-- eval (BinApp Add ex1 ex2) = testInfinite ((evalHelper ex1) + (evalHelper ex2))
-- eval (BinApp Sub ex1 ex2) = testInfinite ((evalHelper ex1) - (evalHelper ex2))
-- eval (BinApp Mul ex1 ex2) = testInfinite ((evalHelper ex1) * (evalHelper ex2))
-- eval (BinApp Div ex1 ex2) = testInfinite (div (evalHelper ex1) (evalHelper ex2))
-- eval (Num x) = Just x
-- evalHelper :: Expr -> Int
-- evalHelper (BinApp Add ex1 ex2) = (evalHelper ex1) + (evalHelper ex2)
-- evalHelper (BinApp Sub ex1 ex2) = (evalHelper ex1) - (evalHelper ex2)
-- evalHelper (BinApp Mul ex1 ex2) = (evalHelper ex1) * (evalHelper ex2)
-- evalHelper (BinApp Div ex1 ex2) = div (evalHelper ex1) (evalHelper ex2)
-- evalHelper (Num x)              = x
-- test = eval (BinApp Div (Num 2) (Num 1))
