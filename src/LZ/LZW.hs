{- | 
  Module      : LZW
  Description : An implementation of LZW method
  Maintainer  : ???
-}

module LZ.LZW (compressLZW, uncompressLZW) where

import Data.Maybe (fromMaybe)
import Data.List (findIndex, elemIndex)
import Debug.Trace (trace)
import LZ.Dictionaries


-- | LZW compressLZW method
compressLZW :: String -> [Int]
compressLZW textToCompress = encode textToCompress ascii "" 256 []
    where
        encode :: String -> [String] -> String -> Int -> [Int] -> [Int]
        encode [] dict w _ result = 
            case findIndex (== w) dict of
                  Just index -> result ++ [index] 
                  Nothing -> error "Compression: not found in dictionary" 
        encode (c:cs) dict w nextCode result =
          let p = w ++ [c]
          -- in trace ("dict: " ++ ", p: " ++ show p ++ ", w: " ++ show w ++ ", [c]: " ++ show [c]) $
          in case findIndex (== p) dict of
                  Just index ->  {-trace ("p (" ++ p ++ ") found in the dictionnary") $-} encode cs dict p nextCode result
                  Nothing ->
                      -- trace ("p (" ++ p ++ ") not found in the dictionnary") $
                      let code = fromMaybe (error "Compression: not found in dictionary") (findIndex (== w) dict)
                          newDict = dict ++ [p]
                          -- dict' = dict ++ ["\\" ++ show nextCode]
                      in --trace ("[code]: " ++ show [code] ++ "dict' : " ++ show newDict) $
                         encode cs newDict [c] (nextCode + 1) (result ++ [code])

-- -- | LZW uncompressLZW method
uncompressLZW :: [Int] -> String 
uncompressLZW arrayToUnCompress = decode arrayToUnCompress ascii 0 "" 256 "" 
    where
        decode :: [Int] -> [String] -> Int -> String -> Int -> String -> String 
        decode [] dict w _ _ result = result
        decode (c:cs) dict w prevString nextCode result
            | c >= 0 && c < length dict =
                let currentChar = dict !! c
                    combinedChar = prevString ++ currentChar
                in case elemIndex combinedChar dict of
                    Just index ->
                               -- trace ("\nWITHIN INDEX DICT -> compressed INT : " ++ show c ++ " | dict length : " ++ show (length dict) ++ " | findIndex : " ++ show index ++ " | combinedChar : " ++ combinedChar ++ " | prevString : " ++ prevString ++ "  |  current Char : " ++ currentChar ++ " | char add to dict : " ++ (prevString ++ [head currentChar]) ++ " | prevIndex : " ++ show prevIndex) $
                               -- trace ("DictionaryJUST: " ++ show (dict ++ [prevString ++ [head currentChar]])) $
                               decode cs dict w currentChar nextCode (result ++ currentChar)
                    Nothing -> -- trace ("\nNOTHING -> compressed INT : " ++ show c ++ " | dict length : " ++ show (length dict) ++ " | prevString : " ++ prevString ++ "  |  current Char : " ++ currentChar ++ " | char add to dict : " ++ (prevString ++ [head currentChar]) ++ " | prevIndex : " ++ show prevIndex) $
                               -- trace ("DictionaryNOTH: " ++ show (dict ++ [prevString ++ [head currentChar]])) $
                               decode cs (dict ++ [prevString ++ [head currentChar]]) c currentChar nextCode (result ++ currentChar)

            | otherwise =
                let chaine = prevString ++ [head prevString]
                    newDict = dict ++ [chaine]
                -- trace ("\nWITHIN OHTERWISE -> compressed INT : " ++ show c ++ " | dict length : " ++ show (length dict) ++ " | prevString : " ++ prevString ++ "  |  current Char : " ++ " | char add to dict (prevString + impossible de ajouter currentChar) : " ++ prevString ++ " | prevIndex : " ++ show prevIndex) $
                -- trace ("DictionaryOTHER: " ++ show (dict ++ ([prevString ++ prevString]))) $
                in decode cs newDict c chaine nextCode (result ++ chaine)
            where 
                currentChar = dict !! c
                prevIndex = getIndexOrZero prevString dict


getIndexOrZero :: Eq a => a -> [a] -> Int 
getIndexOrZero x xs = case elemIndex x xs of 
                        Just index -> index + 1
                        Nothing -> 0