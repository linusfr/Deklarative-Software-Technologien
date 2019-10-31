data Attr =
  Attr String String

data XML
  = Text String
  | Node String [Attr] [XML]

pretty :: XML -> String
pretty (Text text) = text
pretty (Noade name as children) =
  open name as ++ concat (pretty children) ++ close name

prettyAttr :: Atr -> String
prettyAttr (Attr name value) = name ++ "=\"" ++ value ++ "\""

open :: String -> [Attr] -> String
open name as = "<" ++ name ++ unwords (map prettyAttr as) ++ ">"

close :: String -> String
close name = "</" ++ name ++ ">"

-- Anmerkung concat
concat :: [[a]] -> [a]
-- a = Char
concat :: [[Char]] -> [Char]
-- [Char] = String
concat :: [String] -> String
