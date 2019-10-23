module SVG where

data Attr =
  Attr String String
  deriving (Show)

data XML
  = Tag String [Attr] [XML]
  | TextNode String
  deriving (Show)

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
