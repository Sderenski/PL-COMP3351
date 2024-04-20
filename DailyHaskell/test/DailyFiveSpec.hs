module DailyFiveSpec where

    import Test.Hspec

    import DailyFive

    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        describe "multPairs" $ do
            context "multPairs [(2,2), (3,2), (3,3)]" $
                it "should be [4,6,9]" $
                    (multPairs [(2,2), (3,2), (3,3)]) `shouldBe` [4,6,9]
            
            context "multPairs [(1,2)]" $
                it "should be [2]" $
                    (multPairs [(1,2)]) `shouldBe` [2]

            context "multPairs [(5,5), (2,3), (4,4), (6,6)]" $
                it "should be [25, 6, 16, 36]" $
                    (multPairs[(5,5), (2,3), (4,4), (6,6)]) `shouldBe` [25, 6, 16, 36]

        describe "squareList" $ do
            context "squareList [1,3,2]" $
                it "should be [(1,1), (3,9), (2,4)]" $
                    (squareList [1,3,2]) `shouldBe` [(1,1), (3,9), (2,4)]
            
            context "squareList [3]" $
                it "should be [(3,9)]" $
                    (squareList [3]) `shouldBe` [(3,9)]
            
            context "squareList [2,38,58]" $
                it "should be [(2,4), (38,1444), (58,3364)]" $
                    (squareList [2,38,58]) `shouldBe` [(2,4), (38,1444), (58,3364)]

        describe "findLowercase" $ do
            context "findLowercase ['hi','Hi','Bye']" $
                it "should be [True, False, False]" $
                    (findLowercase ["hi","Hi","Bye"]) `shouldBe` [True, False, False]
            context "findLowercase ['Goodbye','hH','HH']" $
                it "should be [False, True, False]" $
                    (findLowercase ["Goodbye","hH","HH"]) `shouldBe` [False, True, False]

            context "findLowercase ['h','H','High']" $
                it "should be [True, False, False]" $
                    (findLowercase ["h","H","High"]) `shouldBe` [True, False, False]
            