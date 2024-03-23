{- |
  Module      : LZ.LZ78
  Description : An implementation of LZ78 method
  Maintainer  : ???
-}
module LZ.LZ78(compress, uncompress) where

import LZ.Dictionaries
import Data.List (elemIndex, findIndex, isPrefixOf)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)


-- debug
str = "belle echelle"
dict = ["", "bel", "lle","foobar","be"]


-- | LZ78 compress method
compress :: String -> [(Int, Char)]
compress [] = []
compress str = compress' str empty []


-- arguments
  -- String : la chaine à compreser
  -- Dictionary : le dictionaire
  -- [(Int,Char)] : la party déjà compresser

compress' :: String -> Dictionary -> [(Int, Char)] ->  [(Int, Char)]
compress' [] _ compressed = compressed
compress' str dict compressed = []
  -- let 
  --   k = prefixLengths str dict 
  --   maxi = maximum k
  --   index =  findIndex (== maxi) k 
  --   txt = take (maxi+1) str
  --   newStr = drop maxi+1 str
  -- in
  --   compress' newStr (dict ++ [txt]) (compressed ++ [index, last txt])


countCommonPrefix :: String -> String -> Int
countCommonPrefix [] _ = 0
countCommonPrefix _ [] = 0
countCommonPrefix (x:xs) (y:ys)
  | x == y = 1 + countCommonPrefix xs ys
  | otherwise = 0

prefixLengths :: String -> [String] -> [Int]
prefixLengths str dict = map (countCommonPrefix str) dict

-- | LZ78 uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(Int, Char)] -> Maybe String
uncompress [] = Just []
uncompress compressed = uncompress' "" empty compressed

-- renvoit un fragment à partir d'un tuple (Int, Char)
  -- String : chaine décompresser
  -- Dictionary : le dictionaire
  -- [(Int, Char)] : le reste du message compresser
uncompress' :: String -> Dictionary -> [(Int, Char)] -> Maybe String
uncompress' decoded dict [] = Just decoded
uncompress' decoded dict ((index, char) : rest) = 
  if index < 0 || index >= length dict 
    then Nothing
    else
      let txt = (dict !! index) ++ [char] in
      uncompress' (decoded ++ txt) (dict ++ [txt]) rest 
