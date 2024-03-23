{- |
  Module : Statistic.EncodingTree
  Description : A module representing a binary tree for binary encoding
  Maintainer : ???
-}
module Statistic.EncodingTree(EncodingTree(..), isLeaf, count, has, encode, decodeOnce, decode, meanLength, compress, uncompress) where

import Statistic.Bit

data EncodingTree a = EncodingNode Int (EncodingTree a) (EncodingTree a)
                    | EncodingLeaf Int a
  deriving (Eq, Show)

-- | Is the encoding a mere leaf ?
isLeaf :: EncodingTree a -> Bool
isLeaf (EncodingLeaf _ _) = True
isLeaf  _                 = False

-- | The length of the underlying source
count :: EncodingTree a -> Int
count (EncodingLeaf cnt _  ) = cnt
count (EncodingNode cnt _ _) = cnt

-- | Search for symbol in encoding tree
has :: Eq a => EncodingTree a -> a -> Bool
(EncodingLeaf _ x) `has` y = x == y
(EncodingNode _ left right) `has` y = left `has` y || right `has` y

-- | Computes the binary code of symbol using encoding tree
-- If computation is not possible, returns `Nothing`.
encode :: Eq a => EncodingTree a -> a -> Maybe [Bit]
encode (EncodingLeaf _ x) y = Just [Statistic.Bit.Zero]
encode tree symbol = encodeHelper tree symbol []
  where
    encodeHelper :: Eq a => EncodingTree a -> a -> [Bit] -> Maybe [Bit]
    encodeHelper (EncodingLeaf _ x) y path
      | x == y    = Just (reverse path)
      | otherwise = Nothing
    encodeHelper (EncodingNode _ left right) y path =
      case (encodeHelper left y (Zero:path), encodeHelper right y (One:path)) of
        (Just p, _) -> Just p
        (_, Just p) -> Just p
        _           -> Nothing


-- | Computes list of symbols from list of bits using encoding tree
decode :: EncodingTree a -> [Bit] -> Maybe [a]
decode _ [] = Just []  -- Si la liste de bits est vide, retourne une liste vide
decode tree bits = case decodeOnce tree bits of
    Just (symbol, remainingBits) -> case decode tree remainingBits of  -- Si un symbole est decompresse avec succes, decompresse recursivement le reste des symboles
        Just symbols -> Just (symbol : symbols)
        Nothing -> Nothing
    Nothing -> Nothing

decode' :: EncodingTree a -> [Bit] -> Maybe [a]
decode' _ [] = Just []  -- Si la liste de bits est vide, retourne une liste vide
decode' tree bits = case decodeOnce' tree bits of
    Just (symbol, remainingBits) -> case decode' tree remainingBits of  -- Si un symbole est decompresse avec succes, decompresse recursivement le reste des symboles
        Just symbols -> Just (symbol : symbols)
        Nothing -> Nothing
    Nothing -> Nothing

-- | Decode a single symbol using the given encoding tree
decodeOnce :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce (EncodingLeaf _ symbol) bits = Just (symbol, bits)
decodeOnce (EncodingNode _ left right) (bit:bits) = case bit of
    Zero -> decodeOnce left bits  -- Si le bit est 0, cherche le symbole dans le sous-arbre gauche
    One -> decodeOnce right bits  -- Si le bit est 1, cherche le symbole dans le sous-arbre droit
decodeOnce _ _ = Nothing

decodeOnce' :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce' (EncodingLeaf _ symbol) (b:bits) = Just (symbol, bits)
decodeOnce' (EncodingNode _ left right) (bit:bits) = case bit of
    Zero -> decodeOnce' left bits  -- Si le bit est 0, cherche le symbole dans le sous-arbre gauche
    One -> decodeOnce' right bits  -- Si le bit est 1, cherche le symbole dans le sous-arbre droit
decodeOnce' _ _ = Nothing

-- | Mean length of the binary encoding
meanLength :: EncodingTree a -> Double
meanLength tree = totalLength / fromIntegral totalFrequency
  where
    (totalLength, totalFrequency) = traverseTree 0 0 tree

    traverseTree :: Int -> Int -> EncodingTree a -> (Double, Int)
    traverseTree depth freq (EncodingLeaf f _) = (fromIntegral (depth * f), f)
    traverseTree depth freq (EncodingNode f left right) = (leftLength + rightLength, leftFreq + rightFreq)
      where 
        (leftLength, leftFreq) = traverseTree (depth + 1) freq left
        (rightLength, rightFreq) = traverseTree (depth + 1) freq right



-- | Compress method using a function generating encoding tree and also returns generated encoding tree
compress :: Eq a => ([a] -> Maybe (EncodingTree a)) -> [a] -> (Maybe (EncodingTree a), [Bit])
compress treeGenerator input =
  case treeGenerator input of
    Nothing   -> (Nothing, [])
    Just tree -> case encodedInput of
                   Nothing -> (Nothing, [])
                   Just bits -> (Just tree, concat bits)
                   where encodedInput = mapM (encode tree) input


-- | Uncompress method using previously generated encoding tree
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: (Maybe (EncodingTree a), [Bit]) -> Maybe [a]
uncompress (Nothing, _) = Nothing
uncompress (Just tree, bits) = uncompress' tree bits

-- | Recursive helper function for uncompression
uncompress' :: EncodingTree a -> [Bit] -> Maybe [a]
uncompress' _ [] = Just []
uncompress' (EncodingLeaf x y) bit = case decode' (EncodingLeaf x y) bit of
    Just value -> Just value
    Nothing -> Nothing
uncompress' tree bits = case decode tree bits of  -- Utilise decode pour decompresser les symboles
    Just symbols -> Just symbols
    Nothing -> Nothing