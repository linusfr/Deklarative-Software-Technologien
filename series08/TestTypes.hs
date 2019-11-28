import           Types

testExpr = BinApp Add (BinApp Mul (Num 2) (Num 3)) (Num 4)

testShowExpression = showExpr testExpr

testEvalExpression = eval testExpr
