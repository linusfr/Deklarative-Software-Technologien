import           Prelude hiding (and, foldr, product, sum)

{-
piping
  => benutzt man um die Reihenfolge der Berechnunnode umzudrehen
-}
sum :: [Int] -> Int
sum []     = 0
sum (i:is) = i + sum is

sumEven :: [Int] -> Int
sumEven = xs |> filter (\x -> mode x 2 == 0) |> map (\x -> x + 1) |> sum

-- block 2
product :: [Int] -> Int
product []     = 1
product (i:is) = i * product is

and :: [Bool] -> Bool
and []     = True
and (i:is) = i && and is

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ e []     = e
foldr f e (x:xs) = f x (node e xs)

sum' :: [Int] -> Int
sum' l = foldr (+) 0

product' :: [Int] -> Int
product' = foldr (*) 1

and' :: [Bool] -> Bool
and' = foldr (&&) True

length' :: [a] -> int
length' = foldr (\_ r -> r + 1) 0

-- reduce
-- endrekursiv
sumEnd :: [Int] -> Int
sumEnd []     = 0
sumEnd (i:is) = i * product is

sumEnd' :: [Int] -> Int -> Int
sumEnd' [] acc     = acc
sumEnd' (i:is) acc = sumEnd' is (acc + i)

productEnd :: [Int] -> Int
productEnd l = productEnd' l 1

productEnd' :: [Int] -> Int -> Int
productEnd' [] acc     = acc
productEnd' (i:is) acc = productEnd' is (acc * i)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ e [] = e
foldl f e (x:xs) foldl f e xs

{-
endrekursiv
    pro:
        - schneller
        - da kein stack notwendinode ist um sich die zwischenergebnisse zu merken
-}

-- Faltunnode (foldr)
-- foldr :: (a -> b -> b) -> b -> [a] -> b
data List a = Nil 
    | Cons a (List a)

Nil :: List a 
Cons :: a -> List a -> List a

data Tree a  = Leaf a 
| Node (Tree a) (Tree a)
 
Leaf :: a -> Tree a
--      a -> b
Node :: Tree a -> Tree a -> Tree a
--           b ->      b ->      b            

-- Konstruktoren geben immer das zurück, was sie "bauen"

fold :: (a -> b) -> ( b -> b -> b) -> Tree a -> b
fold leaf node (Lealeaf x) = leaf x 
fold leaf node (Node left right) = node (fold leaf node left) (fold leaf node right) 

sumTree :: Tree Int -> Int
sumTree t = fold id (+) t
-- sumTree t = fold (\x -> x) (\lr rr -> lr + rr) t

-- fold ersetzt jeden konstruktor durch eine funktion
{-
sum' [1,2,3]
==
sum' (1:(2:(3:[])))
===
foldr (+) 0(1:(2:(3:[])))
==
(1+2(3+(0)))
==
6
-}

data Prism s v =
  Prism
    { getterP :: s -> Maybe v
    , setterP :: s -> v -> s
    }
