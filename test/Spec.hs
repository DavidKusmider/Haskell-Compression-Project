import RLE
import Test.Hspec
import Control.Monad
import Data.Semigroup

-- if running without stack on windows, run terminal as admin
    -- >choco install cabal 
    -- >cabal update
    -- >cabal install hspec
    -- >cabal install --lib hspec


-- to run test, open terminal in root directory then :
-- >ghci -isrc .\test\Spec.hs
-- ghci> main

-- you can refresh file with :
-- ghci> :l test\Spec.hs 
spec_function :: Spec
spec_function = do 
    describe "Test compression / décompression RLE" $ do
        it "simple string" $
            uncompress (compress "aabbbcdeeee") `shouldBe` (Just "aabbbcdeeee")
        it "ex2 "$
            2 `shouldBe` 2

    describe "Test compression / décompression LZ" $ do
        it "foobar string" $
            3 `shouldBe` 3
        it "bardofdqsf"$
            2 `shouldBe` 2

main :: IO ()
main = hspec spec_function