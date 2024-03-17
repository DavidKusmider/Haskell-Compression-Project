module Main (main) where

import Statistic.ShannonFano
import Statistic.Huffman
import Statistic.EncodingTree
import Statistic.Bit
import LZ.LZ78
import LZ.LZW
import LZ.Dictionaries

import Data.Maybe (fromMaybe)
import System.IO (openFile, hPutStrLn, hClose, IOMode(AppendMode))

-- Fonction pour écrire le résultat de uncompress dans un fichier
writeUncompressedToFile :: FilePath -> String -> IO ()
writeUncompressedToFile filepath content = do
    handle <- openFile filepath AppendMode  -- Ouvre le fichier pour écriture
    hPutStrLn handle content  -- Écrit le contenu dans le fichier
    hClose handle  -- Ferme le fichier

main :: IO ()
main = do

    putStrLn "Veuillez choisir une technique de compression :"
    putStrLn "1. Shannon-Fano"
    putStrLn "2. Huffman"
    putStrLn "3. LZ78"
    putStrLn "4. LZW"
    choice <- getLine
    case choice of
        -- "1" -> do
        --     let inputString = "this is an example of a shannon tree"
        --     compressWith (compress treeShannonFano inputString) inputString

        -- "2" -> do
        --     let inputString = "this is an example of a huffman tree"  
        --     compressWith (compress treeHuffman inputString) inputString
        -- _   -> putStrLn "Choix invalide. Veuillez entrer 1 ou 2."

        "3" -> do
            let str = "abracadabraa"
                dict = [""]
                compressedLZ78 = compressLZ78 str dict
            putStrLn "Chaîne originale :"
            putStrLn str
            putStrLn "Chaîne compressée :"
            print compressedLZ78
            putStrLn "Chaîne decompressée :"
            putStrLn $ "Chaîne decompressée : " ++ uncompressLZ78 compressedLZ78 compressedLZ78


            -- TEST REMOVEPREFIX
            -- putStrLn "Running tests..."
            -- let prefix = "helli"
            --     str = "hello world"
            --     result = removePrefix prefix str
            -- putStrLn result
            -- let prefix = "hello"
            --     str = "hello world"
            --     result = removePrefix prefix str
            -- putStrLn result

            -- -- TEST FIND LONGEST PREFIX
            -- putStrLn "Running tests..."
            -- print $ findLongestPrefix "abc" []
            -- print $ findLongestPrefix "abc" ["a", "ab", "abc", "abcd"]
            -- print $ findLongestPrefix "xyz" ["a", "ab", "abc", "abcd"]
            -- print $ findLongestPrefix "abcd" ["a", "ab", "abc", "abcd"]
            -- putStrLn "Tests complete."

        "4" -> do
            let input = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            let compressed = compressLZW input
            putStrLn $ "\nChaîne initiale : " ++ input ++ "\n"
            putStrLn $ "##################################### START COMPRESS ##################################### "
            putStrLn $ "Chaîne compressée : " ++ show compressed
            putStrLn $ "##################################### END COMPRESS #####################################\n "
            putStrLn $ "##################################### START UNCOMPRESS ##################################### "
            putStrLn $ "Chaîne decompressée : " ++ uncompressLZW compressed
            putStrLn $ "##################################### END UNCOMPRESS #####################################\n "
            



compressWith :: (Maybe (EncodingTree Char), [Bit]) -> String -> IO ()
compressWith (maybeCompressedTree, compressedBits) inputString = do 
    case maybeCompressedTree of
        Nothing -> putStrLn "La construction de l'arbre a échoué."
        Just _ -> do
            putStrLn "\nChaîne de caractères d'origine :\n"
            putStrLn inputString
            putStrLn "\nArbre généré :\n"
            print maybeCompressedTree
            putStrLn "\nChaîne de bits compressée :\n"
            putStrLn $ concatMap show compressedBits
            putStrLn "\nMessage en cours de décompression"
            let maybeValue = uncompress (maybeCompressedTree, compressedBits)
            case maybeValue of
                Nothing -> putStrLn "La valeur est Nothing."
                Just val -> do
                    putStrLn "\nMessage décompressé :\n"
                    putStrLn val
    putStrLn "\nFin du programme"
