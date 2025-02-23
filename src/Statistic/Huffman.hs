{- |
  Module : Statistic.Huffman
  Description : A module containing specifics for the Huffman compression method
  Maintainer : ???
-}
module Statistic.Huffman(treeHuffman) where

import Data.Map (Map, insertWith, empty, toList)
import Statistic.Source
import Statistic.EncodingTree

import Data.List (sortOn)
import Data.Maybe (listToMaybe)

-- | Huffman tree generation
treeHuffman :: Ord a => [a] -> Maybe (EncodingTree a)
treeHuffman [] = Nothing
treeHuffman symbols =
  let leafNodes = sortOn fst $ map (\(s, c) -> (c, EncodingLeaf c s)) $ toList $ occurrences symbols
      huffmanTree = buildHuffmanTree leafNodes
  in listToMaybe huffmanTree

buildHuffmanTree :: [(Int, EncodingTree a)] -> [EncodingTree a]
buildHuffmanTree [] = []
buildHuffmanTree [(_, tree)] = [tree]
buildHuffmanTree ((freq1, tree1) : (freq2, tree2) : rest) =
  let newNode = EncodingNode (freq1 + freq2) tree1 tree2
  in buildHuffmanTree $ sortOn fst ((freq1 + freq2, newNode) : rest)