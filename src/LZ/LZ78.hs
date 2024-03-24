{- |
  Module      : LZ.LZ78
  Description : An implementation of LZ78 method (without \0 cheat for end of sequences)
  Maintainer  : ???
-}
module LZ.LZ78(compress, uncompress, subStrLen) where

import LZ.Dictionaries
import Data.List (elemIndex, findIndex, isPrefixOf)
import Data.Maybe (fromMaybe)


-- debug
-- import Debug.Trace (traceShow)
-- import Debug.Trace
-- str = "belle echelle !"
-- dict = ["", "bel", "lle","foobar","be"]


-- | LZ78 compress method
compress :: String -> [(Int, Char)]
compress [] = []
compress str = compress' str empty []


-- arguments
  -- String : la chaine à compresser
  -- Dictionary : le dictionaire
  -- [(Int,Char)] : la partie déjà compressee
compress' :: String -> Dictionary -> [(Int, Char)] ->  [(Int, Char)]
compress' [] _ compressed = compressed
compress' str dict compressed = compress' (drop (maxi + 1) str) (dict ++ [newStr]) (compressed ++ [(index, str !! maxi)])
  where
    k = map (\s -> subStrLen str s) dict -- calcul longueur sous chaines de str qui correspondent a des elements du dictionnaire
    --on utilise subStrLen pour tout element s (eventuel substring de str) du dictionnaire
    -- k c'est comme si on refaisait un dictionnaire
    maxi = case length str == maximum k of --if the last sequence of the string is already in the dictionnary
              True -> maximum k - 1 --on considere qu'on utilise pas le prefixe le plus long (mais le 2eme plus long)
              False -> maximum k
    index = case elemIndex maxi k of
              Just v -> v 
              Nothing -> -1 --theorically unreachable code bc maxi is ALWAYS in k per definition
    newStr = take (maxi + 1) str --nouveau prefixe a ajouter au dictionnaire



-- renvoit un fragment à partir d'un tuple (Int, Char)
  -- String : chaine décompressee
  -- Dictionary : le dictionaire
  -- [(Int, Char)] : le reste du message compresse
uncompress :: [(Int, Char)] -> Maybe String
uncompress [] = Just []
uncompress compressed = uncompress' "" empty compressed
  where
    uncompress' :: String -> Dictionary -> [(Int, Char)] -> Maybe String
    uncompress' decoded dict [] = Just decoded
    uncompress' decoded dict ((index, char) : rest) =
      case index < 0 || index >= length dict of
        True -> Nothing
        False -> uncompress' (decoded ++ txt) (dict ++ [txt]) rest
          where
            txt = (dict !! index) ++ [char]




-- FONCTION UTILITAIRE

-- if substring is the beginning of string, return the length of the substring, else, return 0
subStrLen :: String -> String -> Int
subStrLen str substr = subStrLen' 0 str substr 

subStrLen' :: Int -> String -> String -> Int
subStrLen' acc _ [] = acc --si on a fini de parcourir substring, a ce moment il est bien prefixe donc on retourne la longueur (acc) trouvee
subStrLen' acc [] _ = 0 --si string est vide on return 0
subStrLen' acc (x:xs) (y:ys)
    | x == y = subStrLen' (acc+1) xs ys --si x==y alors on ajoute 1 a la longueur du prefixe trouve et on continue
    -- de chercher un prefixe plus long
    | otherwise = 0 --sinon en fait substr n'est pas un prefixe donc l'index vaut 0
