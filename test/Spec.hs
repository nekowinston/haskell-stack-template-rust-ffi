import qualified Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Rust ffi" $ do
    it "add works correctly" $ do
      Lib.add 2 2 `shouldBe` (4 :: Int)

    it "add works with arbitrary Integers" $
      property $
        \a b -> Lib.add a b == (a + b :: Int)
