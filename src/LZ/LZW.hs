{- |
  Module      : LZW
  Description : An implementation of LZW method
  Maintainer  : ???
-}

module LZ.LZW (compress, uncompress) where

import Data.Maybe (fromMaybe)
import Data.List (findIndex, elemIndex)
import Debug.Trace
import LZ.Dictionaries


-- | LZW compress method
compress :: String -> [Int]
compress textToCompress = encode textToCompress ascii "" 256 []
  where
    encode :: String -> [String] -> String -> Int -> [Int] -> [Int]
    encode [] dict w _ result = 
        case findIndex (== w) dict of
          Just index -> result ++ [index]
          Nothing    -> []
    encode (c:cs) dict w nextCode result =
        case findIndex (== p) dict of
          Just index -> encode cs dict p nextCode result
          Nothing    -> encode cs newDict [c] (nextCode + 1) (result ++ [code])
      where
        p = w ++ [c]
        code = fromMaybe (error "not found") (findIndex (== w) dict)
        newDict = dict ++ [p]


-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> Maybe String 
uncompress arrayToUnCompress = Just $ decode arrayToUnCompress ascii "" "" 
  where
    decode :: [Int] -> [String] -> String -> String -> String 
    decode [] dict  _  result = 
       result
    decode (c:cs) dict prevString result
      | c >= 0 && c < length dict = 
          case elemIndex combinedChar dict of -- on cherche combinedChar dans le dictionnaire
            Just index -> decode cs dict currentChar (result ++ currentChar)
            Nothing    -> decode cs (dict ++ [prevString ++ [head currentChar]]) currentChar (result ++ currentChar)
      | otherwise = decode cs newDict chaine (result ++ chaine)
      where 
        currentChar = dict !! c -- extraction de l'element à l'indice c
        combinedChar = prevString ++ currentChar
        chaine = prevString ++ [head prevString] -- cas otherwise
        newDict = dict ++ [chaine] -- cas otherwise


