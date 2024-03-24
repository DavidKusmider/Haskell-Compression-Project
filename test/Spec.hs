import RLE
import LZ.LZ78
import LZ.LZW
import Statistic.Huffman
import Statistic.ShannonFano
import Statistic.EncodingTree


--import needed for test
import Test.Hspec
import Control.Monad
import Data.Semigroup

--import needed for reading test file testdata.txt
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text as Text
import Data.Text.IO as Text
import Text.Printf


-- you need Hspecs and Text librairies to run test
-- use either cabal or stack to install dependecies
-- if you only have ghc installed :
    -- >choco install cabal 
    -- >cabal update
    -- >cabal install hspec
    -- >cabal install --lib hspec
    -- >cabal install --lib text


-- to run test as standalon, open terminal in root directory then :
-- $ghci -isrc
-- ghci>:set -package containers
-- ghci>:l test\Spec.hs
-- ghci>main


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
            it "LZ78 "$
                LZ.LZ78.uncompress (LZ.LZ78.compress inputAsString) `shouldBe` (Just inputAsString)
            it "LZW "$
                LZ.LZW.uncompress (LZ.LZW.compress inputAsString) `shouldBe` (Just inputAsString)
            it "ShanonFano "$
                Statistic.EncodingTree.uncompress (Statistic.EncodingTree.compress Statistic.ShannonFano.treeShannonFano inputAsString) `shouldBe` (Just inputAsString)
            it "Huffman "$
                Statistic.EncodingTree.uncompress (Statistic.EncodingTree.compress Statistic.Huffman.treeHuffman inputAsString) `shouldBe` (Just inputAsString)
 

main :: IO ()
main = hspec spec_function