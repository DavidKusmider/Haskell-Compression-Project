{- |
  Module : Statistic.EncodingTree
  Description : A module representing a binary tree for binary encoding
  Maintainer : ???
-}
module Statistic.EncodingTree(EncodingTree(..), isLeaf, count, has, encode, decodeSymbol, decode, meanLength, compress, uncompress) where

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
decode tree bits = case decodeSymbol tree bits of
    Just (symbol, remainingBits) -> case decode tree remainingBits of  -- Si un symbole est decompresse avec succes, decompresse recursivement le reste des symboles
        Just symbols -> Just (symbol : symbols)
        Nothing -> Nothing
    Nothing -> Nothing

-- | Decode a single symbol using the given encoding tree
decodeSymbol :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeSymbol (EncodingLeaf _ symbol) bits = Just (symbol, bits)
decodeSymbol (EncodingNode _ left right) (bit:bits) = case bit of
    Zero -> decodeSymbol left bits  -- Si le bit est 0, cherche le symbole dans le sous-arbre gauche
    One -> decodeSymbol right bits  -- Si le bit est 1, cherche le symbole dans le sous-arbre droit
decodeSymbol _ _ = Nothing


-- | Mean length of the binary encoding
meanLength :: EncodingTree a -> Double
meanLength tree = fromIntegral (sumLengths tree) / fromIntegral (count tree)
  where
    sumLengths :: EncodingTree a -> Int
    sumLengths (EncodingLeaf c _)     = c
    sumLengths (EncodingNode c left right) = (c + sumLengths left + sumLengths right)


-- | Compress method using a function generating encoding tree and also returns generated encoding tree
compress :: Eq a => ([a] -> Maybe (EncodingTree a)) -> [a] -> (Maybe (EncodingTree a), [Bit])
compress treeGenerator input =
  case treeGenerator input of
    Nothing   -> (Nothing, [])
    Just tree -> let encodedInput = mapM (encode tree) input in
                 case encodedInput of
                   Nothing -> (Nothing, [])
                   Just bits -> (Just tree, concat bits)


-- | Uncompress method using previously generated encoding tree
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: (Maybe (EncodingTree a), [Bit]) -> Maybe [a]
uncompress (Nothing, _) = Nothing
uncompress (Just tree, bits) = uncompress' tree bits

-- | Recursive helper function for uncompression
uncompress' :: EncodingTree a -> [Bit] -> Maybe [a]
uncompress' _ [] = Just []
uncompress' (EncodingLeaf x y) bit = case decodeSymbol (EncodingLeaf x y) bit of
    Just value -> Just [fst value]
uncompress' tree bits = case decode tree bits of  -- Utilise decode pour decompresser les symboles
    Just symbols -> Just symbols
    Nothing -> Nothing