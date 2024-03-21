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


-- Compress with dictionary trace
compress :: String -> [Int]
compress textToCompress = encode textToCompress ascii "" 256 []
  where
    encode :: String -> [String] -> String -> Int -> [Int] -> [Int]
    encode [] dict w _ result = 
      trace ("Dictionary: " ++ show dict) $
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

-- Uncompress with dictionary trace
uncompress :: [Int] -> String 
uncompress arrayToUnCompress = decode arrayToUnCompress ascii 0 "" 256 "" 
  where
    decode :: [Int] -> [String] -> Int -> String -> Int -> String -> String 
    decode [] dict w _ _ result = 
      trace ("Dictionary: " ++ show dict) $
        result
    decode (c:cs) dict w prevString nextCode result
      | c >= 0 && c < length dict = 
          case elemIndex combinedChar dict of
            Just index -> decode cs dict w currentChar nextCode (result ++ currentChar)
            Nothing    -> decode cs (dict ++ [prevString ++ [head currentChar]]) c currentChar nextCode (result ++ currentChar)
      | otherwise = decode cs newDict c chaine nextCode (result ++ chaine)
      where 
        currentChar = dict !! c
        combinedChar = prevString ++ currentChar
        chaine = prevString ++ [head prevString]
        newDict = dict ++ [chaine]
