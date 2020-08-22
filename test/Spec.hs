import Test.Hspec
import Test.QuickCheck
import TicTac

emptyGameTest :: Grid
emptyGameTest = [[Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing]]

main :: IO ()
main = hspec $ do
  describe "TicTac Test" $ do
    it "Empty Grid" $ do
      emptyGrid `shouldBe` emptyGameTest
