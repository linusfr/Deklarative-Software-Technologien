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
pretty :: XML -> String
