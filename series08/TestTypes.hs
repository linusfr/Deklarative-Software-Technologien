import           Types

testExpr = BinApp Add (BinApp Mul (Num 2) (Num 3)) (Num 4)

testShowExpression = showExpr testExpr

testEvalExpression = eval testExpr

testXML =
  (Tag
     "html"
     []
     [ (Tag "head" [] [Tag "title" [] [TextNode "PageTitle"]])
     , (Tag
          "body"
          [(Attr "class" "content"), (Attr "id" "main")]
          [ Tag "h1" [] [TextNode "A Heading"]
          , Tag "p" [] [TextNode "A paragraph"]
          ])
     ])

testPretty = pretty testXML

createHTML :: IO ()
createHTML = writeFile "testPretty2.html" testPretty
