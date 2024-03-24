{- |
  Module      : LZ.LZ78
  Description : An implementation of LZ78 method
  Maintainer  : ???
-}
module LZ.LZ78bis(compress, uncompress, subStrLen) where

import LZ.Dictionaries
import Data.List (elemIndex, findIndex, isPrefixOf)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)


-- debug
import Debug.Trace
str = "belle echelle !"
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
compress' str dict compressed = 
    let
        k = map (\s -> subStrLen str s) dict 
        maxi = if (length str) == (maximum k) --if the last sequence of the string is already in the dictionnary
            then maximum k -1
            else maximum k
        index = case findIndex (==maxi) k of
            Just v -> v 
            Nothing -> -1 --theorically unreachable code bc maxi is ALWAYS in k per definition
        newStr = take (maxi+1) str
    in
        --traceShow(dict) dbug
        compress' (drop (maxi+1) str) (dict ++ [newStr]) (compressed ++ [(index, (str !! maxi))] )



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


-- FONCTION UTILITAIRE


-- if substring is the beginning of string, return the length of the substring, else, return 0
subStrLen :: String -> String -> Int
subStrLen str substr = subStrLen' 0 str substr 

subStrLen' :: Int -> String -> String -> Int
subStrLen' acc _ [] = acc
subStrLen' acc [] _ = 0
subStrLen' acc (x:xs) (y:ys)
    | x == y = subStrLen' (acc+1) xs ys 
    | otherwise = 0
