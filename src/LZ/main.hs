module Main where

import LZW

main :: IO ()
main = do
    -- let input = "Le lorem ipsum est, en imprimerie, une suite de mots sans signification utilisée à titre provisoire pour calibrer une mise en page, le texte définitif venant remplacer le faux-texte dès qu'il est prêt ou que la mise en page est achevée. Généralement, on utilise un texte en faux latin, le Lorem ipsum ou Lipsum."
    -- let input = "HASKELL C'est de la merde !!!"
    -- let input = "le Saut\n de \n ligne a \n l'air de \n fonctionne"
    -- let input = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
    -- let input = "ababcbababaaaaaaa"
    let input = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    
    let compressed = compress input

    putStrLn $ "\nChaîne initiale : " ++ input ++ "\n"


    putStrLn $ "##################################### START COMPRESS ##################################### "

    putStrLn $ "Chaîne compressée : " ++ show compressed

    putStrLn $ "##################################### END COMPRESS #####################################\n "

    putStrLn $ "##################################### START UNCOMPRESS ##################################### "

    putStrLn $ "Chaîne decompressée : " ++ uncompress compressed

    putStrLn $ "##################################### END UNCOMPRESS #####################################\n "
