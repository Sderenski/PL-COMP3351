module DailyEightSpec where

    import Test.Hspec

    import DailyEight

    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        describe "findSmallest" $ do
            context "findSmallest [7,5,6,3,1,6,8]" $
                it "should be Just 1" $
                    findSmallest [7,5,6,3,1,6,8] `shouldBe` Just 1

            context "findSmallest [6]" $
                it "should be Just 6" $
                    findSmallest [6] `shouldBe` Just 6

            context "findSmallest [7,5,6,6,8]" $
                it "should be Just 5" $
                    findSmallest [7,5,6,6,8] `shouldBe` Just 5

        describe "allTrue" $ do
            context "allTrue [True, False, False]" $
                it "should be Just False" $
                    allTrue [True, False, False] `shouldBe` Just False

            context "allTrue [True, False, False]" $
                it "should be Just False" $
                    allTrue [True, True, True] `shouldBe` Just True

            context "allTrue [True, False, False]" $
                it "should be Just False" $
                    allTrue [True] `shouldBe` Just True

        describe "countAllVotes" $ do
            context "countAllVotes [Just True]" $
                it "should be (0,1,0)" $
                    countAllVotes [Just True] `shouldBe` (0,1,0)

            context "countAllVotes [Just True, Nothing, Nothing, Nothing]" $
                it "should be (3,1,0)" $
                    countAllVotes [Just True, Nothing, Nothing, Nothing] `shouldBe` (3,1,0)

            context "countAllVotes [Just True, Just False, Nothing, Just False, Just True, Just True]" $
                it "should be (1,3,2)" $
                    countAllVotes [Just True, Just False, Nothing, Just False, Just True, Just True] `shouldBe` (1,3,2)