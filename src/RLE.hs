{- |
  Module      : RLE
  Description : An implementation of the run-length encoding method
  Maintainer  : ???
-}
module RLE(compress, uncompress) where

-- | RLE compress method
compress :: Eq a => [a] -> [(a, Int)]
compress [] = []  -- Si la liste est vide, retourne une liste vide
compress (x:xs) = compress' xs x 1  -- Commence la compression en appelant compress' avec le premier élément de la liste et un compteur initialisé à 1

-- Fonction auxiliaire pour la compression RLE
compress' :: Eq a => [a] -> a -> Int -> [(a, Int)]
compress' [] sym count = [(sym, count)]  -- Si la liste est vide, retourne un seul tuple avec le symbole et son compteur
compress' (x:xs) sym count
  | x == sym = compress' xs sym (count + 1)  -- Si le symbole suivant est identique au symbole actuel, incrémente le compteur et continue la compression
  | otherwise = (sym, count) : compress' xs x 1  -- Si le symbole suivant est différent, ajoute le tuple actuel à la liste résultante et commence à compter à partir du nouveau symbole


-- | RLE uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(a, Int)] -> Maybe [a]
uncompress [] = Just []  -- Si la liste de tuples est vide, retourne une liste vide
uncompress ((sym, count):rest) = case uncompress rest of  -- Décompresse récursivement le reste de la liste de tuples
    Just symbols -> Just (replicate count sym ++ symbols)  -- Concatène la répétition du symbole avec le reste des symboles décompressés
    Nothing -> Nothing  -- En cas d'échec de la décompression récursive, retourne Nothing
