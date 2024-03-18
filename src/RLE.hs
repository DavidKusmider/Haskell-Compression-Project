{- |
  Module      : RLE
  Description : An implementation of the run-length encoding method
  Maintainer  : ???
-}
module RLE(compress, uncompress) where

-- | RLE compress method
compress :: Eq a => [a] -> [(a, Int)]
compress [] = []
compress (x:xs) = compress' xs x 1

-- Fonction auxiliaire pour la compression RLE
compress' :: Eq a => [a] -> a -> Int -> [(a, Int)]
compress' [] sym count = [(sym, count)]
compress' (x:xs) sym count
  | x == sym = compress' xs sym (count + 1)
  | otherwise = (sym, count) : compress' xs x 1


-- | RLE uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(a, Int)] -> Maybe [a]
uncompress [] = Just []
uncompress ((sym, count):rest) = case uncompress rest of 
    Just symbols -> Just (replicate count sym ++ symbols) 
    Nothing -> Nothing
