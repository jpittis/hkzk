import Test.Hspec

main :: IO ()
main = hspec $
  describe "Lib" $
    it "has a working test suite" $
      True `shouldBe` True
