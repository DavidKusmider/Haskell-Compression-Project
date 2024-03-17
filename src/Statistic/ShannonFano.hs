{- | 
  Module : Statistic.ShannonFano
  Description : A module containing specifics for the Shannon-Fano compression method
  Maintainer : RAHMANI Kevin
-}
module Statistic.ShannonFano(treeShannonFano) where

import Statistic.EncodingTree
import Statistic.Source (orderedCounts)

-- | Shannon-Fano tree generation
treeShannonFano :: Ord a => [a] -> Maybe (EncodingTree a)
treeShannonFano symbols =
    case orderedCounts symbols of
        [] -> Nothing  -- Aucun symbole dans la liste
        counts -> Just $ buildTree counts
  where
    -- Construction récursive de l'arbre d'encodage Shannon-Fano
    buildTree :: Ord a => [(a, Int)] -> EncodingTree a
    buildTree [] = error "La liste des symboles ne devrait pas être vide"
    buildTree [(sym, occurrence)] = EncodingLeaf occurrence sym  -- Base case: leaf with a symbol
    buildTree counts =
        let
            (leftCounts, rightCounts) = splitCounts counts  -- Divide the counts into two parts
            leftSubtree = buildTree leftCounts  -- Build left subtree recursively
            rightSubtree = buildTree rightCounts  -- Build right subtree recursively
        in
            EncodingNode (sumCounts counts) leftSubtree rightSubtree  -- Node with left and right subtrees

    -- Divise les comptages en deux parties équilibrées
    splitCounts :: [(a, Int)] -> ([(a, Int)], [(a, Int)])
    splitCounts counts = splitHelper counts [] []
      where
        -- Fonction auxiliaire pour la répartition équilibrée des occurrences
        splitHelper :: [(a, Int)] -> [(a, Int)] -> [(a, Int)] -> ([(a, Int)], [(a, Int)])
        splitHelper [] left right = (left, right)
        splitHelper (x:xs) left right =
          let
            leftSum = sumCounts left
            totalSum = (leftSum + sumCounts (x:xs)) `div` 2 
            -- Choix de la liste où ajouter le tuple en fonction des sommes CHECKER SI <= OU < 
            (newLeft, newRight) = if leftSum  + sumCounts [x] <= totalSum then (left ++ [x], right) else (left, right ++ [x])
          in
            splitHelper xs newLeft newRight

    -- Calcule la somme des comptages dans une liste de paires (symbole, comptage)
    sumCounts :: [(a, Int)] -> Int
    sumCounts = sum . map snd


