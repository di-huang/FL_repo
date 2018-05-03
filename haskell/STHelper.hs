module SfxTree where

type SForest a = [SfxTree a]

data SfxTree a =
  SNode {
    ev :: a ,  -- edgeValue
    frst :: SForest a  -- forest
  }
  deriving Show

-- get the tail part of a string using "tail [a]" function

--whether  arg strA is the substring of arg strB
isPrefix :: String -> String ->  Bool
isPrefix "" strB = True
isPrefix strA "" = False
isPrefix (x:xs) (y:ys)
  | x == y = isPrefix xs ys
  | otherwise = False

ipc_helper :: String -> String -> Int -> Int
ipc_helper a "" i = i
ipc_helper "" b i = i
ipc_helper (x:xs) (y:ys) i
  | x == y = ipc_helper xs ys (i + 1)
  | otherwise = i

ipc :: String -> String -> Int
ipc a b = ipc_helper a b 0

hasOneChild :: SfxTree a -> Bool
hasOneChild (SNode x ft)
  | length ft == 1 = True
  | otherwise = False

compress' :: SfxTree String -> SfxTree String
compress' (SNode (s:ss) [ st ]) = SNode ((s:ss) ++ (ev st)) (frst st)
compress' (SNode s (t:ts)) = (SNode s (map compress' (t:ts)))
