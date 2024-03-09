module Main (main) where

import Statistic.Source
import Statistic.EncodingTree
import Statistic.Huffman

import Data.Maybe(fromMaybe)
import Data.Typeable(typeOf)

import System.IO

main :: IO ()
main = do
    -- Chaîne de caractères à compresser
    let inputString = "this is an example of a huffman tree"

    -- Construction de l'arbre de Huffman à partir de la chaîne de caractères
    let huffmanTree = tree inputString

    -- Compression de la chaîne de caractères en utilisant l'arbre de Huffman
    let (maybeCompressedTree, compressedBits) = Statistic.EncodingTree.compress tree inputString
    -- Gestion des cas où la compression échoue ou où l'arbre de Huffman n'est pas généré avec succès
    case maybeCompressedTree of
        Nothing -> putStrLn "La compression a échoué ou l'arbre de Huffman n'a pas été généré avec succès."
        Just _ -> do
            -- Affichage des résultats si la compression a réussi
            putStrLn "Chaîne de caractères d'origine :"
            putStrLn inputString
            putStrLn "Arbre de Huffman généré :"
            print maybeCompressedTree
            putStrLn "Chaîne de bits compressée :"
            putStrLn $ concatMap show compressedBits
            putStrLn "message en cours de decompression"
            print $ typeOf maybeCompressedTree
            print $ typeOf compressedBits
            let maybeValue = Statistic.EncodingTree.uncompress (maybeCompressedTree, compressedBits)
            print $ typeOf maybeValue
            putStrLn "message decompresse"
            case maybeValue of
                Nothing -> putStrLn "La valeur est Nothing."
                Just val -> do
                    putStrLn "Message ecrit"
                    putStrLn val
    putStrLn "Fin du programme"