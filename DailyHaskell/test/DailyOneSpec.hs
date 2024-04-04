module DailyOneSpec where

  import Test.Hspec
  import DailyOne

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do
    describe "quadratic" $ do
      context "quadratic 0 0 0 1" $
        it "should be 0" $
          (quadratic 0 0 0 1) `shouldBe` 0

      context "quadratic 0 0 1 0" $
        it "should be 0" $
          (quadratic 0 0 1 0) `shouldBe` 0

      context "quadratic 0 1 0 0" $
        it "should be 0" $
          (quadratic 0 1 0 0) `shouldBe` 0

      context "quadratic 1 0 0 0" $
        it "should be 1" $
          (quadratic 1 0 0 0) `shouldBe` 1

    describe "scaleVector" $ do
      context "scaleVector 5 (1,0)" $
        it "should be (5, 0)" $
          (scaleVector 5 (1, 0)) `shouldBe` (5, 0)

      context "scaleVector 10 (0,1)" $
        it "should be (0, 10)" $
          (scaleVector 10 (0, 1)) `shouldBe` (0, 10)

      context "scaleVector 0 (1,1)" $
        it "should be (0, 0)" $
          (scaleVector 0 (0, 0)) `shouldBe` (0, 0)

      context "scaleVector 3 (2,3)" $
        it "should be (6, 9)" $
          (scaleVector 3 (2, 3)) `shouldBe` (6, 9)

    describe "tripleDistance" $ do
      context "tripleDistance (0,0,1) (0,0,0)" $
        it "should be 1.0" $
          (tripleDistance (0, 0, 1) (0, 0, 0)) `shouldBe` 1.0 

      context "tripleDistance (0,0,1) (0,0,-1)" $
        it "should be 2.0" $
          (tripleDistance (0, 0, 1) (0, 0, -1)) `shouldBe` 2.0 

      context "tripleDistance (0,0,1) (0,1,0)" $
        it "should be sqrt(2)" $
          (tripleDistance (0, 0, 1) (0, 1, 0)) `shouldBe` 
            (sqrt ((0 - 0)^2 + (0 - 1)^2 + (1 - 0)^2))
