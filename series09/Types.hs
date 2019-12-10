module Types where

{-
Aufgabe 1 - Typen

    Geben Sie möglichst allgemeine Typen für die folgenden Ausdrücke an.
    • λx → x
-}
expr1 :: a -> a
expr1 = (\x -> x)

{-
    • filter (λx → x)
-}
expr2 :: [Bool] -> [Bool]
expr2 = filter (\x -> x)

{-
    Weil filter eine Funktion bekommen muss, welche einen Bool erzeugt, die gegebene Funktion jedoch
    lediglich den Wert zurückgibt den Sie bekommt, muss eine Bool Liste übergeben werden.

    • length ◦ filter (λx → x)
-}
expr3 :: [Bool] -> Int
expr3 = length . filter (\x -> x)

{-
    Die Funktion braucht wieder eine Bool Liste. Weil das Ergebnis von filter an Length übergeben wird, kommt ein int raus.

    • λx _ → x
-}
expr4 :: a -> b -> a
expr4 = (\x _ -> x)

{-
    • (λx → x) True
-}
expr5 :: Bool
expr5 = (\x -> x) True

{-
    identisch zu einfach nur True, da True an ID übergeben wird.

    • map ((λx _ → x) True)
-}
expr6 :: [a] -> [Bool]
expr6 = map ((\x _ -> x) True)

{-
    Der Ausdruck verwirft immer das Listenargument und ersetzt es durch True, es wird eine Liste erwartet und gibt eine Bool Liste zurück.

    • λx → map ((&&) x)
-}
expr7 :: Bool -> [Bool] -> [Bool]
expr7 = (\x -> map ((&&) x))

{-
    expr7Test = (expr7 True) [True, False]
    Die Funktion nimmt ein Bool und gibt eine Funktion zurück welchen von BoolListe nach Bool Liste geht.

    λx → map ((&&) x) [False, False]
-}
expr8 :: Bool -> [Bool]
expr8 = (\x -> map ((&&) x) [False, False])

{-
    Wie expr7, nur eine Bool Liste wurde eigesetzt

    map (λx → map ((&&) x) [False, False])
-}
expr9 :: [Bool] -> [[Bool]]
expr9 = map (\x -> map ((&&) x) [False, False])

{-
    exr9Test = expr9 [True, False]
    Die Funktion nimmt eine Bool Liste. Für jedes Element der Bool Liste wird wird es als x im inneren Map verwendet

    map (\x -> map ((&&) x) [False, False]) [True, False]
    =
    [(\True -> map ((&&) True) [False, False]), (\False -> map ((&&) False) [False, False])]
    ==
    [[(\True -> map ((&&) True False)), (\True -> map ((&&) True False))], [(\True -> map ((&&) True False), (\True -> map ((&&) False False) )]]

    • map (λx → map ((&&) x) [False, False]) [True, True]
-}
expr10 :: [[Bool]]
expr10 = map (\x -> map ((&&) x) [False, False]) [True, True]
{-
    [[(\True -> map ((&&) True False)), (\True -> map ((&&) True False))], [(\True -> map ((&&) True False), (\True -> map ((&&) False False) )]]
    [[False, False], [False, False]]
-}
