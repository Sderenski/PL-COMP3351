module WeeklyThreeSpec where

    import Test.Hspec

    import WeeklyThree

    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        describe "Vec" $ do
            context "show (Vec [1.0,2.0,3.0])" $
                it "should be Vec [1.0,2.0,3.0]" $
                    (show (Vec [1.0, 2.0, 3.0])) `shouldBe` "Vec [1.0,2.0,3.0]"

            context "show (Vec [4.0])" $
                it "should be Vec [4.0]" $
                    (show (Vec [4.0])) `shouldBe` "Vec [4.0]"

            context "show (Vec [7.0,3.0])" $
                it "should be Vec [7.0,3.0]" $
                    (show (Vec [7.0, 3.0])) `shouldBe` "Vec [7.0,3.0]"
            
            context "Vec [1.0,2.0] + Vec [1.0,2.0]" $
                it "should be Vec [2.0,4.0]" $
                    (Vec [1.0,2.0] + Vec [1.0,2.0]) `shouldBe` Vec [2.0,4.0]
            
            context "Vec [4.0,2.0,5.0] + Vec [1.0,2.0,1.0]" $
                it "should be Vec [5.0,4.0,6.0]" $
                    (Vec [4.0,2.0,5.0] + Vec [1.0,2.0,1.0]) `shouldBe` Vec [5.0,4.0,6.0]

            context "Vec [6.0,2.0] + Vec [1.0,10.0]" $
                it "should be Vec [7.0,12.0]" $
                    (Vec [6.0,2.0] + Vec [1.0,10.0]) `shouldBe` Vec [7.0,12.0]

            context "Vec [3.0,3.0] - Vec [1.0,2.0]" $
                it "should be Vec [2.0,1.0]" $
                    (Vec [3.0,3.0] - Vec [1.0,2.0]) `shouldBe` Vec [2.0,1.0]

            context "Vec [0.0,0.0] - Vec [1.0,2.0]" $
                it "should be Vec [-1.0,-2.0]" $
                    (Vec [0.0,0.0] - Vec [1.0,2.0]) `shouldBe` Vec [-1.0,-2.0]
            
            context "Vec [3.0,3.0] - Vec [3.0,3.0]" $
                it "should be Vec [0.0,0.0]" $
                    (Vec [3.0,3.0] - Vec [3.0,3.0]) `shouldBe` Vec [0.0,0.0]

            context "Vec [1.0,2.0] * Vec [1.0,2.0]" $
                it "should be Vec [1.0,4.0]" $
                    (Vec [1.0,2.0] * Vec [1.0,2.0]) `shouldBe` Vec [1.0,4.0]

            context "Vec [5.0,2.0,1.0] * Vec [5.0,2.0,7.0]" $
                it "should be Vec [25.0,4.0,7.0]" $
                    (Vec [5.0,2.0,1.0] * Vec [5.0,2.0,7.0]) `shouldBe` Vec [25.0,4.0,7.0]

            context "Vec [1.0] * Vec [2.0]" $
                it "should be Vec [2.0]" $
                    (Vec [1.0] * Vec [2.0]) `shouldBe` Vec [2.0]

            context "abs (Vec [1.0, -2.0])" $
                it "should be Vec [1.0,2.0]" $
                    (abs (Vec [1.0,-2.0])) `shouldBe` Vec [1.0,2.0]

            context "abs (Vec [-1.0, 2.0])" $
                it "should be Vec [1.0,2.0]" $
                    (abs (Vec [-1.0,2.0])) `shouldBe` Vec [1.0,2.0]

            context "abs (Vec [1.0, 2.0])" $
                it "should be Vec [1.0,2.0]" $
                    (abs (Vec [1.0,2.0])) `shouldBe` Vec [1.0,2.0]

            context "signum (Vec [1.0,2.0])" $
                it "should be Vec [1.0,1.0]" $
                    (signum (Vec [1.0,2.0])) `shouldBe` Vec [1.0,1.0]

            context "signum (Vec [0.0,0.0])" $
                it "should be Vec [0.0,0.0]" $
                    (signum (Vec [0.0,0.0])) `shouldBe` Vec [0.0,0.0]

            context "signum (Vec [-1.0,-2.0])" $
                it "should be Vec [-1.0,-1.0]" $
                    (signum (Vec [-1.0,-2.0])) `shouldBe` Vec [-1.0,-1.0]

            context "Vec [1.0,2.0] == Vec [1.0,2.0]" $
                it "should be True" $
                    (Vec [1.0,2.0] == Vec [1.0,2.0]) `shouldBe` True

            context "Vec [-1.0,2.0] == Vec [-1.0,2.0]" $
                it "should be True" $
                    (Vec [-1.0,2.0] == Vec [-1.0,2.0]) `shouldBe` True

            context "Vec [1.0,-2.0] == Vec [1.0,2.0]" $
                it "should be False" $
                    (Vec [1.0,-2.0] == Vec [1.0,2.0]) `shouldBe` False
            
            context "Vec [1.0,2.0] < Vec [2.0,2.0]" $
                it "should be True" $
                    (Vec [1.0,2.0] < Vec [2.0,2.0]) `shouldBe` True
            
            context "Vec [1.0,2.0] <= Vec [1.0,2.0]" $
                it "should be True" $
                    (Vec [1.0,2.0] <= Vec [2.0,2.0]) `shouldBe` True
            
            context "Vec [8.0,2.0] < Vec [2.0,2.0]" $
                it "should be False" $
                    (Vec [8.0,2.0] < Vec [2.0,2.0]) `shouldBe` False

            context "magnitude (Vec [3.0, 4.0])" $
                it "should be 5.0" $
                    (magnitude (Vec [3.0, 4.0])) `shouldBe` 5.0

            context "magnitude (Vec [3.0,4.0,12.0])" $
                it "should be 13.0" $
                    (magnitude (Vec [3.0,4.0,12.0])) `shouldBe` 13.0

            context "magnitude (Vec [6.0, 8.0])" $
                it "should be 10" $
                    (magnitude (Vec [6.0,8.0])) `shouldBe` 10.0

            context "Vec [1.0,2.0] <> Vec [1.0,2.0] <> Vec [1.0,2.0]" $
                it "should be Vec [3.0,6.0]" $
                    (Vec [1.0,2.0] <> Vec [1.0,2.0] <> Vec [1.0,2.0]) `shouldBe` Vec [3.0,6.0]

            context "Vec [1.0,2.0,3.0] <> Vec [1.0,2.0,3.0] <> Vec [1.0,2.0,3.0]" $
                it "should be Vec [3.0,6.0,9.0]" $
                    (Vec [1.0,2.0,3.0] <> Vec [1.0,2.0,3.0] <> Vec [1.0,2.0,3.0]) `shouldBe` Vec [3.0,6.0,9.0]

            context "Vec [1.0] <> Vec [1.0] <> Vec [1.0]" $
                it "should be Vec [3.0]" $
                    (Vec [1.0] <> Vec [1.0] <> Vec [1.0]) `shouldBe` Vec [3.0]

            context "Vec [1.0,2.0] <> mempty" $
                it "should be Vec [1.0, 2.0]" $
                    (Vec [1.0,2.0] <> mempty) `shouldBe` Vec [1.0,2.0]

            context "Vec [1.0,2.0,9.0] <> mempty" $
                it "should be Vec [1.0, 2.0,9.0]" $
                    (Vec [1.0,2.0,9.0] <> mempty) `shouldBe` Vec [1.0,2.0,9.0]

            context "Vec [1.0] <> mempty" $
                it "should be Vec [1.0]" $
                    (Vec [1.0] <> mempty) `shouldBe` Vec [1.0]

            