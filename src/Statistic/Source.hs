{- | 
  Module : Statistic.Source
  Description : Some utility functions for sources (input messages)
  Maintainer : ???
-}

module Statistic.Source(occurrences, entropy, orderedCounts) where

import Data.Map (Map, insertWith, empty, toList)

-- | The map giving occurrences of each symbol in the source
occurrences :: Ord a => [a] -> Map a Int
occurrences = foldr (\x acc -> insertWith (+) x 1 acc) empty

-- | SHANNON entropy of source
entropy :: Ord a => [a] -> Double
entropy source = negate . sum $ map (\(x, n) -> let p = fromIntegral n / total in if p == 0 then 0 else p * logBase 2 p) symbolFreqs
    where
        total = fromIntegral $ length source
        symbolFreqs = toList $ occurrences source

-- | List of occurrences ordered by count
orderedCounts :: Ord a => [a] -> [(a, Int)]
orderedCounts = sort . toList . occurrences
  where
    sort :: Ord b => [(a, b)] -> [(a, b)]
    sort [] = []
    -- inverser smaller et larger pour aller du plus grand au plus petit
    sort ((x1, y1):xs) = sort smaller ++ [(x1, y1)] ++ sort larger
      where
        smaller = filter (\(_, y) -> y < y1) xs
        larger  = filter (\(_, y) -> y >= y1) xs
