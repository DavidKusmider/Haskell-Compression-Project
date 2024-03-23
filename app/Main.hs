module Main (main) where

import Statistic.ShannonFano 
import Statistic.Huffman
import Statistic.EncodingTree as EncodingTree
import Statistic.Source (entropy)
import Statistic.Bit
import LZ.LZ78
import LZ.LZW as LZW
import LZ.Dictionaries

import RLE

main :: IO ()
main = do

    putStrLn "Veuillez choisir une technique de compression :"
    putStrLn "1. Shannon-Fano"
    putStrLn "2. Huffman"
    putStrLn "3. LZ78"
    putStrLn "4. LZW"
    putStrLn "5. RLE"
    choice <- getLine
    case choice of
        "1" -> do
            let inputString = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
            compressWith (EncodingTree.compress treeShannonFano inputString) inputString
        "2" -> do
            let inputString = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
            compressWith (EncodingTree.compress treeHuffman inputString) inputString
        
        "3" -> do
            let str = "aaaaaaaaaaabbbbbbbbbbbbcccccccccccc"
                dict = empty
                compressedLZ78 = compressLZ78 str dict
            putStrLn "Chaîne originale :"
            putStrLn str
            putStrLn "Chaîne compressée :"
            print compressedLZ78
            putStrLn $ "Chaîne decompressée : " ++ uncompressLZ78 compressedLZ78 compressedLZ78

        "4" -> do
            let input = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
            -- let input = "ababcbababaaaaaaa"
            -- let input = "aaaaaaaaaaabbbbbbbbbbbbcccccccccccc"
            -- let input = "ABD-ABKABD-ABDABDK"
            -- let input = " "
            -- let input = ""
            compressLZW input
        "5" -> do
            let input = "aaaabbcbbb"
            compressRLE input
        _   -> putStrLn "Choix invalide. Veuillez entrer 1 à 5."



compressLZW :: String -> IO ()
compressLZW input = do
    let compressed = LZW.compress input
    
    putStrLn $ "\nChaîne initiale : " ++ input ++ "\n"
    
    putStrLn "##################################### START COMPRESS #####################################"
    putStrLn $ "Chaîne compressée : " ++ show compressed
    putStrLn "##################################### END COMPRESS #####################################\n"
    
    putStrLn "##################################### START UNCOMPRESS #####################################"
    putStrLn $ "Chaîne décompressée : " ++ LZW.uncompress compressed
    putStrLn "##################################### END UNCOMPRESS #####################################\n"

compressRLE :: String -> IO ()
compressRLE input = do
    let compressed = RLE.compress input
    
    putStrLn $ "\nChaîne initiale : " ++ input ++ "\n"
    
    putStrLn "##################################### START COMPRESS #####################################"
    putStrLn $ "Chaîne compressée : " ++ show compressed
    putStrLn "##################################### END COMPRESS #####################################\n"
    
    let maybeValue = RLE.uncompress $ compressed
    case maybeValue of
        Just(value) -> do
            putStrLn "##################################### START UNCOMPRESS #####################################"
            putStrLn $ "Chaîne décompressée : " ++ value
            putStrLn "##################################### END UNCOMPRESS #####################################\n"
        Nothing -> putStrLn input


compressWith :: (Maybe (EncodingTree Char), [Bit]) -> String -> IO ()
compressWith (maybeCompressedTree, compressedBits) inputString = do 
    case maybeCompressedTree of
        Nothing -> putStrLn "La construction de l'arbre a échoué."
        Just tree -> do
            putStrLn "\nChaîne de caractères d'origine :\n"
            putStrLn inputString

            let ent = entropy inputString
            putStrLn $ "\nEntropie : " ++ show ent

            let meanLengthValue = EncodingTree.meanLength tree
            putStrLn $ "\nLongueur moyenne de l'encodage binaire : " ++ show meanLengthValue

            putStrLn "\nArbre généré :\n"
            print tree

            putStrLn "\nChaîne de bits compressée :\n"
            putStrLn $ concatMap show compressedBits

            putStrLn "\nMessage en cours de décompression"
            let maybeValue = EncodingTree.uncompress (maybeCompressedTree, compressedBits)

            case maybeValue of
                Nothing -> putStrLn "La valeur est Nothing."
                Just val -> do
                    putStrLn "\nMessage décompressé :\n"
                    putStrLn val
    putStrLn "\nFin du programme"

