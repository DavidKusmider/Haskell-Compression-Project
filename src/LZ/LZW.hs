{- |
  Module      : LZ.LZW
  Description : An implementation of LZW method
  Maintainer  : ???
-}

module LZ.LZW (compress, uncompress) where

import Data.Maybe (fromMaybe)
import Data.List (findIndex, find)
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



-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> String 
uncompress arrayToUnCompress = decode arrayToUnCompress ascii 0 "" 256 "" 
    where
        decode :: [Int] -> [String] -> Int -> String -> Int -> String -> String 
        decode [] dict w _ _ result = result
        decode (c:cs) dict w prevString nextCode result
          | c >= 0 && c < length dict = decode cs (dict ++ [prevString ++ currentChar]) c currentChar nextCode (result ++ currentChar)
          | otherwise = decode cs (dict ++ [prevString ++ currentChar]) c currentChar nextCode (result ++ currentChar)
          where 
            currentChar = dict !! c

main :: IO ()
main = do
    let input = "aaaabaaaa"
    
    let compressed = compress input
    putStrLn $ "Chaîne compressée : " ++ show compressed

    putStrLn $ "##################################### COMPRESSION TERMINE ##################################### "

    putStrLn $ "Chaîne decompressée : " ++ uncompress compressed
