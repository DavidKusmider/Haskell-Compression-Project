{- |
  Module      : LZ.LZW
  Description : An implementation of LZW method
  Maintainer  : ???
-}

module LZ.LZW (compress, uncompress) where

import Data.Maybe (fromMaybe)
import Data.List (findIndex, find, elemIndex)
import Data.Char (chr)
import Data.List (singleton)
import Debug.Trace (trace)


-- A dictionary is merely an indexed sequence of string
type Dictionary = [String]

-- | The empty dictionary
empty :: Dictionary
empty = [""]

-- | The ASCII dictionary
ascii :: Dictionary
ascii = map (singleton . chr) [0..255]



-- | LZW compress method
compress :: String -> [Int]
compress textToCompress = encode textToCompress ascii "" 256 []
    where
        encode :: String -> [String] -> String -> Int -> [Int] -> [Int]
        encode [] dict w _ result = case findIndex (== w) dict of
                                          Just index -> result ++ [index]
                                          Nothing -> error "Compression: not found in dictionary"
        encode (c:cs) dict w nextCode result =
          let p = w ++ [c]
          in trace ("dict: " ++ ", p: " ++ show p ++ ", w: " ++ show w ++ ", [c]: " ++ show [c]) $
            case findIndex (== p) dict of
                  Just index -> trace ("p (" ++ p ++ ") found in the dictionnary") $ encode cs dict p nextCode result
                  Nothing ->
                      trace ("p (" ++ p ++ ") not found in the dictionnary") $
                      let code = fromMaybe (error "Compression: not found in dictionary") (findIndex (== w) dict)
                          newDict = dict ++ [p]
                          -- dict' = dict ++ ["\\" ++ show nextCode]
                      in trace ("[code]: " ++ show [code] ++ "dict' : " ++ show newDict) $
                         encode cs newDict [c] (nextCode + 1) (result ++ [code])



-- -- | LZW uncompress method
-- -- If input cannot be uncompressed, returns `Nothing`
-- uncompress :: [Int] -> String 
-- uncompress arrayToUnCompress = decode arrayToUnCompress ascii 0 "" 256 "" 
--     where
--         decode :: [Int] -> [String] -> Int -> String -> Int -> String -> String 
--         decode [] dict w _ _ result = result
--         decode (c:cs) dict w prevString nextCode result
--             | c >= 0 && c < length dict =
--                 let currentChar = dict !! c
--                 in case findIndex (== prevString) dict of
--                      Just _ ->  trace ("FIND INDEX -> compressed INT : " ++ show c ++ " | prevString : " ++ prevString ++ "  |  current Char : " ++ currentChar) $
--                                 trace ("Dictionary: " ++ show (dict ++ [prevString ++ currentChar])) $
--                                decode cs dict w currentChar nextCode result
--                      Nothing -> trace ("NOTHING -> compressed INT : " ++ show c ++ " | prevString : " ++ prevString ++ "  |  current Char : " ++ currentChar) $
--                                 trace ("Dictionary: " ++ show (dict ++ [prevString ++ currentChar])) $
--                                 decode cs (dict ++ [prevString ++ currentChar]) c currentChar nextCode (result ++ currentChar)
--             | otherwise =
--                 trace ("Dictionary: " ++ show (dict ++ [prevString ++ currentChar])) $
--                 trace ("OTHERWISE -> compressed INT : " ++ show c ++ " | prevString : " ++ prevString ++ "  |  current Char : " ++ currentChar) $
--                 decode cs (dict ++ [prevString ++ currentChar]) c currentChar nextCode (result ++ currentChar)
--             where 
--                 currentChar = dict !! c

-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> String 
uncompress arrayToUnCompress = decode arrayToUnCompress ascii 0 "" 256 "" 
    where
        decode :: [Int] -> [String] -> Int -> String -> Int -> String -> String 
        decode [] dict w _ _ result = result
        decode (c:cs) dict w prevString nextCode result
            | c >= 0 && c < length dict =
                let currentChar = dict !! c
                    combinedChar = currentChar ++ prevString
                in case findIndex (== combinedChar) dict of
                    Just _ ->
                               trace ("\nWITHIN INDEX DICT -> compressed INT : " ++ show c ++ " | prevString : " ++ prevString ++ "  |  current Char : " ++ currentChar ++ " | prevIndex : " ++ show prevIndex) $
                               decode cs dict w currentChar nextCode (result ++ currentChar)
                    Nothing -> trace ("\nNOTHING -> compressed INT : " ++ show c ++ " | dict length : " ++ show (length dict) ++ " | prevString : " ++ prevString ++ "  |  current Char : " ++ currentChar ++ " | char add to dict : " ++ (prevString ++ [head currentChar]) ++ " | prevIndex : " ++ show prevIndex) $
                               trace ("DictionaryNOTH: " ++ show (dict ++ [prevString ++ [head currentChar]])) $
                               decode cs (dict ++ [prevString ++ [head currentChar]]) c currentChar nextCode (result ++ currentChar)

            | otherwise =
                trace ("\nWITHIN OHTERWISE -> compressed INT : " ++ show c ++ " | dict length : " ++ show (length dict) ++ " | prevString : " ++ prevString ++ "  |  current Char : " ++ " | char add to dict (prevString + impossible de ajouter currentChar) : " ++ prevString ++ " | prevIndex : " ++ show prevIndex) $
                trace ("DictionaryOTHER: " ++ show (dict ++ ([prevString ++ "b"]))) $
                decode cs (dict ++ [prevString]) c (prevString ++ "b") nextCode (result ++ (prevString ++ "b"))
            where 
                currentChar = dict !! c
                prevIndex = getIndexOrZero prevString dict

getIndexOrZero :: Eq a => a -> [a] -> Int 
getIndexOrZero x xs = case elemIndex x xs of 
                        Just index -> index + 1
                        Nothing -> 0

main :: IO ()
main = do
    let input = "ababcbababaaaaaaa"
    let input = "david davide davide"
    
    let compressed = compress input
    putStrLn $ "Chaîne compressée : " ++ show compressed

    putStrLn $ "##################################### COMPRESSION TERMINE ##################################### "

    putStrLn $ "Chaîne decompressée : " ++ uncompress compressed
