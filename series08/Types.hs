module Types where

{-
Aufgabe 4 - Faltungen für algebraische Datentypen
    In dieser Aufgabe sollen Sie sich mit Faltungen für beliebige algebraische Datentypen beschäftigen.
    Gehen Sie dabei jeweils nach dem Schema aus der Vorlesung vor.

    • Definieren Sie eine Faltung für den Datentyp Expr.
-}
data BinOp
  = Add
  | Mul
  deriving (Show)

data Expr
  = BinApp BinOp Expr Expr
  | Num Int
  deriving (Show)

{-
    Konstruktoren
        BinApp :: BinOp -> Expr -> Expr -> Expr
        Num :: Int -> Expr
-}
foldExpr :: (BinOp -> b -> b -> b) -> (Int -> b) -> Expr -> b
foldExpr _ num (Num value) = num value
foldExpr binApp num (BinApp binOp expr1 expr2) =
  binApp binOp (foldExpr binApp num expr1) (foldExpr binApp num expr2)
 {-
    • Implementieren Sie eine Funktion showExpr :: Expr → String mit Hilfe der Faltung.
-}

-- showExpr :: Expr -> String
-- showExpr (BinApp op ex1 ex2) =
--   "(" ++ showExpr ex1 ++ display op ++ showExpr ex2 ++ ")"
--   where
--     display Add = " + "
--     display Mul = " * "
-- showExpr (Num x) = show x
showExpr :: Expr -> String
showExpr =
  foldExpr
    (\op expr1 expr2 -> "(" ++ expr1 ++ display op ++ expr2 ++ ")")
    (\x -> show x)
  where
    display Add = " + "
    display Mul = " * "

{-
    • Implementieren Sie eine Funktion eval0 :: Expr → Int mit Hilfe der Faltung.
-}
eval :: Expr -> Int
eval = foldExpr (\op exp1 exp2 -> applyOp op exp1 exp2) (\x -> x)
  where
    applyOp Add = (+)
    applyOp Mul = (*)

{-
    • Definieren Sie für den Datentyp XML eine Faltung.
-}
data Attr =
  Attr String String
  deriving (Show)

data XML
  = Tag String [Attr] [XML]
  | TextNode String
  deriving (Show)

{-
Konstruktoren
  Tag :: String -> [Attr] -> [XML] -> XML
  TextNode :: String -> XML

  =>

  foldXML :: (String -> b) -> (String -> [Attr] -> [XML] -> b) -> XML -> b
-}
foldXML :: (String -> a) -> (String -> [Attr] -> [a] -> a) -> XML -> a
foldXML text tag (TextNode str) = text str
foldXML text tag (Tag name attrs xmls) =
  tag name attrs (map (foldXML text tag) xmls)

{-
• Implementieren Sie die Funktion pretty aus der 4. Übung mit Hilfe der Faltung.
-}
pretty :: XML -> String
pretty xml = foldXML id prettyNode xml
  where
    prettyNode name attrs strs =
      "<" ++
      name ++
      " " ++
      unwords (map prettyAttr attrs) ++
      ">" ++ concat strs ++ "</" ++ name ++ ">"

prettyAttr :: Attr -> String
prettyAttr (Attr name value) = name ++ "=\"" ++ value ++ "\""
