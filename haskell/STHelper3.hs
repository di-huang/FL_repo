module Test4 where

data Tree a =
    Root [Tree a] | Node a [Tree a] | Leaf a
    deriving (Show, Eq)

source :: Tree String
source =
    Root
        [Node "a" [Node "b" [Leaf "c" , Leaf "c0"]], Node "a0" [Node "b0" [Node "c0" [Leaf "d"]], Leaf "b1"]]

strip :: Tree String -> Tree String
strip (Node a [Leaf b]) = Leaf $ a ++ b
strip (Node a [Node b c]) = Node (a ++ b) c
strip (Root a) = Root $ map strip a
strip (Node a b) = Node a $ map strip b
strip (Leaf a) = Leaf a

stripRepeat :: Tree String -> Tree String
stripRepeat a =
    if strip a == a then a else stripRepeat $ strip a

main :: IO ()
main = do
    putStrLn $ "from " ++ show source
    putStrLn $ "to " ++ show (stripRepeat source)

-- get the string value of the tree
getV :: Tree String -> String
getV (Root b) = undefined
getV (Leaf a) = a
getV (Node a b) = a

-- extract the first of the string
-- extract the tail part of the string using the function "tail"
head' :: String -> String
head' "" = ""
head' (x:xs) = [x]

-- insert the first of string into the forest of the input tree anyhow
insert0 :: String -> Tree String -> Tree String
insert0 s (Root b) = Root $ (Leaf $ head' s) : b
insert0 s (Leaf a) = Node a $ [Leaf $ head' s]
insert0 s (Node a b) = Node a $ (Leaf $ head' s) : b

-- compare the first of the string with the string value of the tree
compare' :: String -> Tree String -> Bool
compare' "" t = False
compare' s (Root b) = False
compare' s (Node a b) = if (head' s) == a then True else False
compare' s (Leaf a) = if (head' s) == a then True else False

-- whether or not in the forest
inside :: String -> Tree String -> Bool
inside "" t = undefined
inside s (Root []) = False
inside s (Root (t:ts)) = if s == (getV t) then True else inside s (Root ts)
inside s (Node a []) = False
inside s (Node a (t:ts)) = if s == (getV t) then True else inside s (Node a ts)

