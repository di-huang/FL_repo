module SfxTree where

import Data.List

-- Project (sample topic 2) : Suffix Tree (Bioinformatics) 
-- Group Members: dhuang(Di Huang), yhe28(Yifan He)
data Tree a =
    Root [Tree a] | Leaf a | Leaf' (a , Int) | Node a [Tree a]
    deriving (Eq , Show)

instance Functor Tree where
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node a b) = Node (f a) ((fmap . fmap) f b)
    fmap f (Root b) = Root ((fmap . fmap) f b)
    
-- single string to char
sc :: String -> Char
sc [c] = c

-- char to single string
cs :: Char -> String
cs c = [c]

-- reference: Homework2/Main.hs
printShowable :: Show a => a -> IO ()
printShowable l =
  do
    putStrLn $ show $ l

-- the helper function of icp
icpHelper :: String -> String -> Int -> Int
icpHelper a "" i = i
icpHelper "" b i = i
icpHelper (x:xs) (y:ys) i
  | x == y = icpHelper xs ys (i + 1)
  | otherwise = i

-- get the starting index of the common prefix
icp :: String -> String -> Int
icp a b = icpHelper a b 0

-- get the string value of the tree
getData :: Tree String -> String
getData (Leaf a) = a
getData (Node a b) = a

-- compare the first char between two strings
compareHead :: String -> String -> Bool
compareHead "" "" = True
compareHead "" b = False
compareHead a "" = False
compareHead a b = if (head a) == (head b) then True else False

-- convert Tree Char to Tree String
treeCS :: Tree Char -> Tree String
treeCS trie = fmap cs trie

-- compress: "a" - "b" -...  ->  "ab" -...
compress :: Tree String -> Tree String
compress (Node a [Leaf b]) = Leaf $ a ++ b
compress (Node a [Node b c]) = Node (a ++ b) c
compress (Root a) = Root $ map compress a
compress (Node a b) = Node a $ map compress b
compress (Leaf a) = Leaf a

-- compress loop function
compressLoop :: Tree String -> Tree String
compressLoop a =
    if compress a == a then a else compressLoop $ compress a

-- parse a string to the list of string ("ab" -> ["$", "b$", "ab$"]). 
parse :: String -> [String]
parse s = reverse $ parseHelper s
  where
    parseHelper "" = ["$"]
    parseHelper s = s : (parseHelper $ tail s) 

-- parse2 a string to the list of string ("ab" -> ["b", "ab"]) without '$' sign.
parse2 :: String -> [String]
parse2 s = reverse $ parseHelper2 s
  where 
    parseHelper2 [c] = [[c]]
    parseHelper2 s = s : (parseHelper2 $ tail s) 

-- convert the trie to the tree
trieToTree :: Tree Char -> Tree String
trieToTree trie = compressLoop $ treeCS $ trie

-- [ Approach 1 ] to build the suffix trie according to the parsed input string --
-- the helper function of makeNode and  sfxtrie1 functions
head_ :: [String] -> (String , [(Char , String)])
head_ ss = (front , rear)
  where
    go [] = ('$' , [])
    go (c:cs) = (c,cs)
    rear = map go ss
    front = nub $ map fst rear

-- the helper function of sfxtrie1
makeNode :: [(Char , String)] -> Char -> Tree Char
makeNode _ '$' = Leaf '$'
makeNode rear n = Node n $ map (makeNode tailr_) ns_
  where
    tailS_ = map snd $ filter ((== n) . fst) rear
    (ns_, tailr_) = head_ tailS_

-- the main function of the approach 1 to build the suffix trie
sfxtrie1 :: [String] -> Tree Char
sfxtrie1 ss = Root $ map (makeNode rest) nodes
  where
    (nodes , rest) = head_ ss

-- build the suffix tree using [ Approach 1 ]
sfxtree1 :: String -> Tree String
sfxtree1 s = compressLoop $ treeCS $ sfxtrie1 $ parse s
-- Approach 1 (above)----------------------------

-- [ Approach 2 ] to build the suffix trie according to the string (no need to parse) --
type Spell = String
-- insert the suffix into the suffix trie one by one
insertTrie :: Spell -> Tree Char -> Tree Char
insertTrie [] (Root trees) = Root $ Leaf '$' : trees
insertTrie allSpell@(spell:spells) (Root trees) | hasSpell spell trees  = Root $ map (goTree allSpell) trees
                                             | otherwise = Root $ (Node spell $ insert_ spells []):trees
  where goTree (spell:spells) (Node v children) | spell == v = Node v $ insert_ spells children
        goTree _ tree = tree

        hasSpell :: Char -> [Tree Char] -> Bool
        hasSpell  spell trees = any (trieEq spell) trees
          where trieEq spell (Node v _) = spell == v
                trieEq _ _ = False

        insert_ :: Spell ->  [Tree Char] -> [Tree Char]
        insert_ []  trees = (Leaf '$'):trees
        insert_ allSpell@(spell:spells) trees | hasSpell spell trees = map (goTree allSpell) trees
                                       | otherwise = (Node spell $ insert_ spells []):trees

-- the main function of the approach 2 to build the suffix trie automatically
sfxtrie2 :: [String] -> Tree Char
sfxtrie2 [] = insertTrie "" $ Root []
sfxtrie2 (x:xs) = insertTrie x (sfxtrie2 xs)

-- build the suffix tree using [ Approach 2 ]. 
sfxtree2 :: String -> Tree String
sfxtree2 s = compressLoop $ treeCS $ sfxtrie2 $ parse2 s
-- Approach 2 (above)----------------------------

-- [ Indexed Suffix Tree ]-----------------------
-- remove the common part (with string b) in string a
rmCom :: String -> String -> String
rmCom "" b = ""
rmCom a "" = a
rmCom (x:xs) (y:ys)
    | x == y = rmCom xs ys
    | otherwise = "*error*"

-- set index for each Leaf
index :: Int -> String -> Tree String -> Tree String
index 0 _ (Leaf "$") = Leaf' ("$" , 0)
index i s (Leaf' d) = Leaf' d
index i s (Root b) = Root $ map (index i s) b
index i s (Node a b)
    | compareHead s a = Node a $ map (index i (rmCom s a)) b
    | otherwise = Node a b
index i "" (Leaf "$") = Leaf' ("$" , i)
index i s (Leaf a) = if (init a) == s then Leaf' (a , i) else Leaf a

-- a loop running index function
indexloop :: [Int] -> [String] -> Tree String -> Tree String
indexloop [] _ t = t
indexloop _ [] t = t
indexloop (i:is) (x:xs) tree = indexloop is xs nextTree
  where
    nextTree = index i x tree

-- get the suffix tree with indexed Leaves
indexTree :: String -> Tree String
indexTree s = indexloop [0..length s] (parse s) $ sfxtree1 s

-- [ Substring Query ] --------------------------
-- check if a string is the substring of a build suffix tree (coming from a source string)
substr_ :: String -> Tree String -> Bool
substr_ "$" t = True
substr_ "" t = True
substr_ s (Root []) = False
substr_ s (Node a []) = False
substr_ s (Leaf a) = if s == a then True else False
substr_ s (Root (x:xs))
  | compareHead (getData x) s = substr_ (drop (icp (getData x) s) s) x
  | otherwise = substr_ s (Root xs)
substr_ s (Node a (x:xs))
  | compareHead (getData x) s = substr_ (drop (icp (getData x) s) s) x
  | otherwise = substr_ s (Node a xs)

-- check if the string a is the substring of the string b by suffix tree (Approach 1)
isSubstr :: String -> String -> Bool
isSubstr a b = if (hasDollar a) then last b == '$' else substr_ a (sfxtree1 b)
  where 
    hasDollar "" = False
    hasDollar (x:xs) = if x == '$' then True else hasDollar xs

-- [ Test functions ] -----------------------------
build :: String -> IO ()
build s = 
  do
    putStrLn "-------------------------Testing-------------------------"
    putStrLn $ "Input String: " ++ s
    putStrLn $ "Parse: " ++ s ++ "$"
    putStrLn $ "Index: " ++ (show $ reverse $ [0..length s])
    putStrLn ""
    putStrLn ">> Building the suffix trie according to the input string..."
    putStrLn "[Approach 1] (constructing directly) --- "
    putStr "suffix trie 1: "
    printShowable $ sfxtrie1 $ parse s
    putStrLn "[Approach 2] (inserting automatically) --- "
    putStr "suffix trie 2: "
    printShowable $ sfxtrie2 $ parse2 s
    putStrLn ""
    putStrLn ">> Converting (suffix trie 1) to (suffix tree 1) using trieToTree function..."
    putStr "suffix tree 1: "
    printShowable $ trieToTree $ sfxtrie1 $ parse s
    putStrLn ""
    putStrLn ">> Building (suffix tree 2) directly using sfxtree2 function..."
    putStr "suffix tree 2: "
    printShowable $ sfxtree2 s
    putStrLn ""
    putStrLn ">> Indexing the suffix tree..."
    printShowable $ indexTree s
    putStrLn ""
