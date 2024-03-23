import RLE
import LZ.LZ78bis
import LZ.LZW
import Statistic.Huffman
import Statistic.ShannonFano
import Statistic.EncodingTree

import Test.Hspec
import Control.Monad
import Data.Semigroup

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text as Text
import Data.Text.IO as Text
import Text.Printf


-- if running without stack on windows, run terminal as admin
    -- >choco install cabal 
    -- >cabal update
    -- >cabal install hspec
    -- >cabal install --lib hspec
    -- >cabal install --lib text


-- to run test, open terminal in root directory then :
-- >ghci -isrc .\test\Spec.hs
-- ghci> main

-- you can refresh file with :
-- ghci> :l test\Spec.hs 


readExamples :: IO [Text]
readExamples = Text.lines <$> Text.readFile "test/testdata.txt"

spec_function :: Spec
spec_function = do 
    examples <- runIO readExamples
    forM_ examples $ \input ->
        let inputAsString = unpack input in
        describe (printf "Testing string : '%s'" inputAsString) $ do
            it "RLE " $
                RLE.uncompress (RLE.compress inputAsString) `shouldBe` (Just inputAsString)
            it "LZ78b "$
                LZ.LZ78bis.uncompress (LZ.LZ78bis.compress inputAsString) `shouldBe` (Just inputAsString)
            it "LZW "$
                 LZ.LZW.uncompress (LZ.LZW.compress inputAsString) `shouldBe` (Just inputAsString)
            it "ShanonFano "$
                Statistic.EncodingTree.uncompress (Statistic.EncodingTree.compress Statistic.ShannonFano.treeShannonFano inputAsString) `shouldBe` (Just inputAsString)
            it "Huffman "$
                Statistic.EncodingTree.uncompress (Statistic.EncodingTree.compress Statistic.Huffman.treeHuffman inputAsString) `shouldBe` (Just inputAsString)
 


main :: IO ()
main = hspec spec_function