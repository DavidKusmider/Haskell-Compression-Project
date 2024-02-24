module Main (main) where

import Statistic.Source
import Statistic.EncodingTree
import Statistic.Huffman

import Data.Maybe(fromMaybe)

import Data.Typeable(typeOf)

import System.IO

-- Fonction pour écrire le résultat de uncompress dans un fichier
writeUncompressedToFile :: FilePath -> String -> IO ()
writeUncompressedToFile filepath list = do
    putStrLn "ouverture du fichier"
    handle <- openFile filepath AppendMode  -- Ouvre le fichier pour écriture
    putStrLn "fichier ouvert"
    hPutStrLn handle list  -- Écrit le contenu dans le fichier
    putStrLn "fermeture du fichier"
    hClose handle  -- Ferme le fichier
    putStrLn "fichier fermer"


main :: IO ()
main = do
    -- Chaîne de caractères à compresser
  let inputString = "Hello, world!"

  -- Construction de l'arbre de Huffman à partir de la chaîne de caractères
  let huffmanTree = tree inputString

  -- Compression de la chaîne de caractères en utilisant l'arbre de Huffman
  let (maybeCompressedTree, compressedBits) = compress tree inputString
  -- Gestion des cas où la compression échoue ou où l'arbre de Huffman n'est pas généré avec succès
  case maybeCompressedTree of
    Nothing -> putStrLn "La compression a échoué ou l'arbre de Huffman n'a pas été généré avec succès."
    Just _ -> do
      -- Affichage des résultats si la compression a réussi
      putStrLn "Chaîne de caractères d'origine :"
      putStrLn inputString
      putStrLn "Arbre de Huffman généré :"
      print huffmanTree
      putStrLn "Chaîne de bits compressée :"
      putStrLn $ concatMap show compressedBits
      putStrLn $ "message en cours de decrompression"
      print $ typeOf(maybeCompressedTree)
      print $ typeOf(compressedBits)
      let maybeValue = uncompress (maybeCompressedTree, compressedBits)
      print $ typeOf(maybeValue)
      putStrLn $ "message decompresse"
      case maybeValue of
        Nothing -> putStrLn $ "La valeur est Nothing."
        Just val -> do
            writeUncompressedToFile "./output.txt" val -- Écrit le résultat dans le fichier "output.txt"
            putStrLn $ "Message ecrit"
      putStrLn "Fin du programme"