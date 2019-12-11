import           ExtendExpr

testExpr = BinApp Add (BinApp Mul (Num 2) (Num 3)) (Num 4)

---------------------------------------
-- eval
---------------------------------------
testEval = eval testExpr
