module Expressions where

{-
Aufgabe 2 - Arithmetische Ausdrücke

    Wir betrachten den folgenden Datentyp zur Darstellung von arithmetischen Ausdrücken.

        data BinOp = Add | Mul
        data Expr = BinApp BinOp Expr Expr | Num Int

    Definieren Sie eine Funktion showExpr :: Expr → String, die einen Ausdruck erhält und eine String-Repräsentation
    des Ausdrucks liefert.
    Anwendungen von Operatoren sollen dabei geklammert werden. Die Funktion showExpr
    liefert für das Argument

    testExpr = BinApp Add (BinApp Mul (Num 2) (Num 3)) (Num 4)

    zum Beispiel das Ergebnis "((2 * 3) + 4)".
-}
data BinOp
  = Add
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
    display Mul = " * "
showExpr (Num x) = show x

{-
    Definieren Sie außerdem eine Funktion eval :: Expr → Int, die einen Ausdruck auswertet. Die Funktion eval liefert
    für den Ausdruck testExpr zum Beispiel das Resultat 10.
-}
eval :: Expr -> Int
eval (BinApp Add ex1 ex2) = eval ex1 + eval ex2
eval (BinApp Mul ex1 ex2) = eval ex1 * eval ex2
eval (Num x)              = x
