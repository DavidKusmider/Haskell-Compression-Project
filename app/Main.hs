module Main (main) where

import Statistic.ShannonFano
import Statistic.EncodingTree
import Statistic.Bit

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
    -- Chaîne de caractères à compresser
    let inputString = "this is an example of a shannon tree"

    -- Compression de la chaîne de caractères en utilisant l'arbre de Shannon-Fano
    let (maybeCompressedTree, compressedBits) = compress tree inputString

    case maybeCompressedTree of
        Nothing -> putStrLn "La construction de l'arbre Shannon-Fano a échoué."
        Just _ -> do
            putStrLn "Chaîne de caractères d'origine :"
            putStrLn inputString
            putStrLn "Arbre de Shannon-Fano généré :"
            print maybeCompressedTree
            putStrLn "Chaîne de bits compressée :"
            putStrLn $ concatMap show compressedBits
            putStrLn "message en cours de decompression"
            let maybeValue = uncompress (maybeCompressedTree, compressedBits)
            putStrLn "message decompresse"
            case maybeValue of
                Nothing -> putStrLn "La valeur est Nothing."
                Just val -> do
                    putStrLn "Message ecrit"
                    putStrLn val
    putStrLn "Fin du programme"

