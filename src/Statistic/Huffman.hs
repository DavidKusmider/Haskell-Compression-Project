{- |
  Module : Statistic.Huffman
  Description : A module containing specifics for the Huffman compression method
  Maintainer : ???
-}
module Statistic.Huffman(tree) where

import Data.Map (Map, insertWith, empty, toList)
import Statistic.Source
import Statistic.EncodingTree

import Data.List (sortOn)
import Data.Maybe (listToMaybe)

-- | Huffman tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree [] = Nothing
tree symbols =
  let leafNodes = sortOn fst $ map (\(s, c) -> (c, EncodingLeaf c s)) $ toList $ occurrences symbols
      huffmanTree = buildHuffmanTree leafNodes
  in listToMaybe huffmanTree

-- | Construit l'arbre de Huffman à partir de la liste de feuilles (triée par fréquence)
buildHuffmanTree :: [(Int, EncodingTree a)] -> [EncodingTree a]
buildHuffmanTree [] = []
buildHuffmanTree [(_, tree)] = [tree]
buildHuffmanTree ((freq1, tree1) : (freq2, tree2) : rest) =
  let newNode = EncodingNode (freq1 + freq2) tree1 tree2
  in buildHuffmanTree $ sortOn fst ((freq1 + freq2, newNode) : rest)