module Query where

-- get the tail part of a string using "tail [a]" function

--whether  arg strA is the substring of arg strB
isPrefix :: String -> String ->  Bool
isPrefix "" strB = True
isPrefix strA "" = False
isPrefix (x:xs) (y:ys)
  | x == y = isPrefix xs ys
  | otherwise = False
