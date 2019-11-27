import           Expressions

----------------------------------------
-- showExpr
----------------------------------------
testExpr = BinApp Add (BinApp Mul (Num 2) (Num 3)) (Num 4)

testExpr' =
  BinApp
    Add
    (BinApp Add (BinApp Add (BinApp Mul (Num 3) (Num 4)) (Num 2)) (Num 2))
    (Num 2)

testShowExpression = showExpr testExpr

testShowExpression' = showExpr testExpr'

----------------------------------------
-- eval
----------------------------------------
testEvalExpression = eval testExpr

testEvalExpression' = eval testExpr'
