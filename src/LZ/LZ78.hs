module LZ.LZ78(compressLZ78, uncompressLZ78, findLongestPrefix, removePrefix) where

import Data.List (elemIndex, findIndex, isPrefixOf)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)



compressLZ78 :: String -> [String] -> [(Int, Maybe Char)]
-- compressLZ78 _ = undefined -- TODO
compressLZ78 [] _ = [] -- Si la chaîne est vide, retourner une liste vide
compressLZ78 str dict = go str dict [] --[] est la chaine compressee
  where
    go [] _ compressed = compressed -- Si le string est vide, retourner la chaîne compressée
    go str dict compressed =
      let prefix = findLongestPrefix str dict
          --RECHERCHE DE L'INDEX
          index = fromMaybe 0 (elemIndex prefix dict)
          --RECHERCHE DU CHAR
          (compressedChar, string') = nextCharNextStringAfterPrefix prefix str
          newDict = dict ++ [concatMaybeChar compressedChar prefix]
          newCompressed = compressed ++ [(index, compressedChar)]
      in traceShow (prefix, newDict, newCompressed) $ go string' newDict newCompressed --traceShow pour l'affichage

uncompressLZ78 :: [(Int, Maybe Char)] -> [(Int, Maybe Char)]  -> String
uncompressLZ78 compressedData compressedTab = go compressedData compressedData [] -- Initialiser la chaine décompressée avec une chaîne vide, compressedtab est compressedData vidé 
  where
    go compressedData [] decompressed = decompressed -- Si on a finit de parcourir compressedTab, retourner la chaîne décompressée
    go compressedData ((index, compressedChar):rest) decompressed =
      let prefix = getPrefixAtIndex index compressedData ++ (concatMaybeChar compressedChar "") -- prefixe + son dernier caractere
          newDecompressed = decompressed ++ prefix -- Concaténer decompressed avec le premier caractère du prefixe et le reste du prefixe
      in go compressedData rest newDecompressed


--FONCTIONS

concatMaybeChar :: Maybe Char -> String -> String --pour concaténer prefixe (String) avec compressedChar(Maybe Char)
concatMaybeChar Nothing str = str -- Si Maybe Char est Nothing, renvoyer simplement la chaîne
concatMaybeChar (Just c) str = str ++ [c] -- Si Maybe Char contient un caractère, concaténer le caractère à la fin de la chaîne

nextCharNextStringAfterPrefix :: String -> String -> (Maybe Char, String) --retourne le nextChar et le restant du string
nextCharNextStringAfterPrefix prefix str = -- on regarde si il y a un char 
    case removePrefix prefix str of
        (c:rest) -> let compressedChar = Just c
                        string' = rest
                    in (compressedChar, string')
        _        -> let compressedChar = Nothing
                        string' = []
                    in (compressedChar, string')


findLongestPrefix :: String -> [String] -> String
findLongestPrefix str [] = "" -- Si le dictionnaire est vide, retourner une chaîne vide
findLongestPrefix str dict = go str "" -- Initialiser avec une chaîne vide
  where
    go [] prefix = prefix -- Si la chaîne est vide, retourner le préfixe trouvé jusqu'à présent
    go (x:xs) prefix
      | (prefix ++ [x]) `elem` dict = go xs (prefix ++ [x]) -- Si le préfixe est présent dans le dictionnaire, le prolonger
      | otherwise = prefix -- Si le préfixe n'est pas présent dans le dictionnaire, retourner le préfixe trouvé jusqu'à présent

-- Retirer un préfixe d'une chaîne
removePrefix :: String -> String -> String
removePrefix prefix str
  | prefix `isPrefixOf` str = drop (length prefix) str
  | otherwise = error "Le prefixe specifie n'est pas present dans la chaine."



getPrefixAtIndex :: Int -> [(Int, Maybe Char)] -> String
getPrefixAtIndex index compressedData = go index compressedData ""
  where
    go 0 _ prefix = prefix -- Quand on a fini
    go index compressedData prefix = 
      case compressedData !! (index-1) of --si on trouve bien quelque chose 
        (i, maybeChar) -> 
          go i compressedData (concatMaybeChar maybeChar "" ++ prefix)
        _ -> error "erreur lors de la decompression"


