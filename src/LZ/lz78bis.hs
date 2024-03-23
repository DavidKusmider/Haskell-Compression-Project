module LZ.LZ78bis(compress, uncompress) where

import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (fromMaybe)
import LZ.Dictionaries


compress :: String -> [(Int, Char)]
compress [] = [] -- Si la chaîne est vide, retourner une liste vide
compress str = go str empty [] -- [] est la chaîne compressée
  where
    go str dict compressed = case string' of
      [] -> newCompressed
      _  -> go string' newDict newCompressed
      where
        prefix = findLongestPrefix str dict
        --RECHERCHE DE L'INDEX
        index = fromMaybe 0 (elemIndex prefix dict)
        --RECHERCHE DU CHAR
        (compressedChar, string') = nextCharNextStringAfterPrefix prefix str
        newDict = dict ++ [concatMaybeChar compressedChar prefix]
        newCompressed = compressed ++ [(index, compressedChar)]


uncompress :: [(Int, Char)] -> Maybe String
uncompress compressedData = go compressedData compressedData [] -- Initialiser la chaîne décompressée avec une chaîne vide, compressedtab égal à compressedData 
  where
    go compressedData [] decompressed = Just decompressed -- Si on a fini de parcourir compressedTab, retourner la chaîne décompressée
    go compressedData [(index, '\0')] decompressed = 
        case getPrefixAtIndex index compressedData of
            Just prefix -> Just (decompressed ++ prefix)
            Nothing -> Nothing -- Retourner Nothing en cas d'erreur dans getPrefixAtIndex
    go compressedData ((index, compressedChar):rest) decompressed =
        case getPrefixAtIndex index compressedData of
            Just prefix -> go compressedData rest (decompressed ++ prefix ++ [compressedChar])
            Nothing -> Nothing -- Retourner Nothing en cas d'erreur dans getPrefixAtIndex







--FONCTIONS

concatMaybeChar :: Char -> String -> String --pour concaténer prefixe (String) avec compressedChar(qui est peut etre '\0')
concatMaybeChar '\0' str = str -- Si Maybe Char est Nothing, renvoyer simplement la chaîne
concatMaybeChar c str = str ++ [c] -- Si Maybe Char contient un caractère, concaténer le caractère à la fin de la chaîne

nextCharNextStringAfterPrefix :: String -> String -> (Char, String) --retourne le nextChar et le restant du string
nextCharNextStringAfterPrefix prefix str = -- on regarde si il y a un char 
    case removePrefix prefix str of
        (c:rest) -> (c, rest)
        _        -> ('\0', [])


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



getPrefixAtIndex :: Int -> [(Int, Char)] -> Maybe String --permet de recup le prefix correspondant a un index
getPrefixAtIndex index compressedData = go index compressedData ""
  where
    go 0 _ prefix = Just prefix -- Quand on a fini
    go index compressedData prefix = --si l'index ne vaut pas 0, on continue de constituer le prefixe
      case compressedData !! (index-1) of --si on trouve bien quelque chose 
        (i, maybeChar) -> 
          go i compressedData (concatMaybeChar maybeChar "" ++ prefix)
        _ -> Nothing