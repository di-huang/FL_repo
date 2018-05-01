module SfxTree where

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
