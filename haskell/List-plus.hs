module More where

import Data.Ord

asc_pre :: (Ord a) => [a] -> [a]
asc_pre [] = []
asc_pre [x] = [x]
asc_pre (x : y : xs)
  | x < y = x : (asc_pre (y : xs))
  | otherwise = [x]

asc_post :: (Ord a) => [a] -> [a]
asc_post [] = []
asc_post [x] = []
asc_post (x : y : xs)
  | x < y = asc_post (y : xs)
  | otherwise = (y : xs)

ascendings :: (Ord a) => [a] -> [[a]]
ascendings [] = []
ascendings [x] = [[x]]
ascendings l = (asc_pre l) : (ascendings (asc_post l))

