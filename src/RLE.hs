{- |
  Module      : RLE
  Description : An implementation of the run-length encoding method
  Maintainer  : ???
-}
module RLE(compress, uncompress) where

-- | RLE compress method
compress :: Eq a => [a] -> [(a, Int)]
compress [] = []
compress (x:xs) = compress' [] xs x 1
--compress (x:xs) = compress' xs x 1

-- Fonction auxiliaire pour la compression RLE
compress' :: Eq a => [(a, Int)] -> [a] -> a -> Int -> [(a, Int)]
compress' compressed [] sym count = compressed ++ [(sym, count)]
compress' compressed (x:xs) sym count
  | x == sym = compress' compressed xs sym (count + 1)
  | otherwise = compress' newCompressed xs x 1
  where newCompressed = compressed ++ [(sym, count)]


-- ancienne version (récursivité non terminal)
-- compress' :: Eq a => [a] -> a -> Int -> [(a, Int)]
-- compress' [] sym count = [(sym, count)]
-- compress' (x:xs) sym count
--   | x == sym = compress' xs sym (count + 1)
--   | otherwise = (sym, count) : compress' xs x 1


-- | RLE uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(a, Int)] -> Maybe [a]
uncompress [] = Just []
uncompress msg = uncompress' [] msg

uncompress' :: [a] -> [(a, Int)] -> Maybe [a]
uncompress' decoded [] = Just decoded
uncompress' decoded ((sym, count):rest) = 
  if count <= 0
   then Nothing
   else uncompress' (decoded ++ replicate count sym) rest

-- ancienne version (récursivité non terminal)
-- uncompress :: [(a, Int)] -> Maybe [a]
-- uncompress [] = Just []
-- uncompress ((sym, count):rest) = case uncompress rest of 
--     Just symbols -> Just (replicate count sym ++ symbols) 
--     Nothing -> Nothing --Unreachable code
