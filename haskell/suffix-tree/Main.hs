module Main where

import SfxTree

-- simple test
main :: IO ()
main = 
  do
    putStrLn "-----------------------Simple Test-----------------------"
    build "abaaba"
    putStrLn $ ">> Substring query using suffix tree (source string: abaaba)..."
    putStrLn "- baa is the substring?"
    printShowable $ isSubstr "baa" "abaaba"
    putStrLn "- aaba is the substring?"
    printShowable $ isSubstr "aaba" "abaaba"
    putStrLn "- aba is the substring?"
    printShowable $ isSubstr "aba" "abaaba"
    putStrLn "- abb is the substring?"
    printShowable $ isSubstr "abb" "abaaba"
    putStrLn "- c is the substring?"
    printShowable $ isSubstr "c" "abaaba"

-- complicated DNA test
main2 :: IO ()
main2 = 
  do
    putStrLn "------------------------DNA Test-------------------------"
    build "gggacaatacgtca"
    putStrLn $ ">> Substring query using suffix tree (source string: gggacaatacgtca)..."
    putStrLn "- ggg is the substring?"
    printShowable $ isSubstr "ggg" "gggacaatacgtca"
    putStrLn "- acg is the substring?"
    printShowable $ isSubstr "acg" "gggacaatacgtca"
    putStrLn "- caata is the substring?"
    printShowable $ isSubstr "caata" "gggacaatacgtca"
    putStrLn "- tacat is the substring?"
    printShowable $ isSubstr "tacat" "gggacaatacgtca"
