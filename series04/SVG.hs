module SVG where

-------------------------------------------------------------
-- Attr
-------------------------------------------------------------
data Attr =
  Attr String String
  deriving (Show)

-------------------------------------------------------------
-- XML
-------------------------------------------------------------
data XML
  = Tag String [Attr] [XML]
  | TextNode String
  deriving (Show)

-------------------------------------------------------------
-- given HTML represented with the data types
-------------------------------------------------------------
--  <html>
--      <head>
--          <title>Page Title</title>
--      </head>
--      <body class="content" id="main">
--          <h1>A Heading</h1>
--          <p>A paragraph</p>
--      </body>
--  </html>
--
wantedDocument =
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

-------------------------------------------------------------
-- pretty
-------------------------------------------------------------
-- use map, concat and unwords to create a string representation of the original html string
--
pretty :: XML -> String
pretty (Tag tagType as [TextNode text]) =
  "<" ++
  tagType ++
  unwords (map (\(Attr x y) -> " " ++ x ++ "=\'" ++ y ++ "\' ") as) ++
  ">" ++ text ++ "</" ++ tagType ++ ">"
pretty (Tag tagType as xs) =
  "<" ++
  tagType ++
  unwords (map (\(Attr x y) -> " " ++ x ++ "=\'" ++ y ++ "\' ") as) ++
  ">" ++ unwords (map pretty xs) ++ "</" ++ tagType ++ ">"

-------------------------------------------------------------
-- pretty'
-------------------------------------------------------------
pretty' :: XML -> String
pretty' (Tag tagType as [TextNode text]) =
  generateTag (Tag tagType as [TextNode text])
pretty' tag = generateTag tag

generateTag :: XML -> String
generateTag (Tag tagType as [TextNode text]) =
  "<" ++ tagType ++ generateAttr as ++ ">" ++ text ++ "</" ++ tagType ++ ">"
generateTag (Tag tagType as xs) =
  "<" ++
  tagType ++
  generateAttr as ++ ">" ++ generateChildren xs ++ "</" ++ tagType ++ ">"

generateAttr :: [Attr] -> String
generateAttr as =
  unwords (map (\(Attr x y) -> " " ++ x ++ "=\'" ++ y ++ "\' ") as)

generateChildren :: [XML] -> String
generateChildren xs = unwords (map pretty xs)

xmlAsString = pretty' wantedDocument

-- write the xml to a html file
createHtmlFile :: IO ()
createHtmlFile = writeFile "wantedHTML.html" xmlAsString

createHtmlFile' :: IO ()
createHtmlFile' = writeFile "wantedHTML'.html" xmlAsString

createAll :: IO ()
createAll = do
  createHtmlFile
  createHtmlFile'
