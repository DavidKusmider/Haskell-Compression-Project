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

-- | Computes the first symbol from list of bits using encoding tree and also returns the list of bits still to process
-- If computation is not possible, returns `Nothing`.
decodeOnce :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce tree bits = decodeHelper tree bits []
  where
    decodeHelper :: EncodingTree a -> [Bit] -> [Bit] -> Maybe (a, [Bit])
    decodeHelper (EncodingLeaf _ x) bs acc = Just (x, bs ++ acc)
    decodeHelper (EncodingNode _ left right) (b:bs) acc =
      case b of
        Zero -> decodeHelper left bs (b:acc)
        One  -> decodeHelper right bs (b:acc)
    decodeHelper _ [] _ = Nothing

-- | Computes list of symbols from list of bits using encoding tree
decode :: EncodingTree a -> [Bit] -> Maybe [a]
decode _ [] = Just []
decode tree bits =
  case decodeOnce tree bits of
    Nothing           -> Nothing
    Just (symbol, bs) -> (symbol :) <$> decode tree bs


-- | Mean length of the binary encoding
meanLength :: EncodingTree a -> Double
meanLength tree = fromIntegral (sumLengths tree) / fromIntegral (count tree)
  where
    sumLengths :: EncodingTree a -> Int
    sumLengths (EncodingLeaf count _)     = count
    sumLengths (EncodingNode _ left right) = sumLengths left + sumLengths right


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
uncompress (Nothing, _)   = Nothing
uncompress (Just tree, bits) = decode tree bits
