module Test2 where

import Data.List (tails)
import Data.Maybe
import Control.Arrow (first, second)
import Data.Map.Strict (Map, empty, insert, insertWith, assocs)

data SfxTree a =
  SLeaf Int |
  SNode [(String, SfxTree a)]
  deriving (Eq , Show)

data SfxTrie a =
  SLeaf_ Int |
  SNode_ (Map (Maybe Char) (SfxTrie a))
  deriving (Eq , Show)

buildTrie :: String -> SfxTrie String
buildTrie str = foldl here (flip const) (init (tails str)) (length str) (SNode_ empty)
  where
  here (~>) ss i (SNode_ snc) = (~>) (i - 1) (SNode_ tr)
    where tr = foldr iter (insert Nothing (SLeaf_ (i - 1))) ss snc
  iter tmp (~>) = (insertWith (==>) (Just tmp) . SNode_) $ (~>) empty
    where _ ==> (SNode_ snc) = SNode_ ((~>) snc)
