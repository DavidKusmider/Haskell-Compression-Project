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
uncompress arrayToUnCompress = Just $ decode arrayToUnCompress ascii 0 "" 256 "" 
  where
    decode :: [Int] -> [String] -> Int -> String -> Int -> String -> String 
    decode [] dict w _ _ result = 
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
