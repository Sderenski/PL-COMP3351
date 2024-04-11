module DailyTwoSpec where
    import Test.Hspec
    import DailyTwo

    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        describe "every4th" $ do
            context "every4th [1,2,3,4,5,6,7,8,9,10]" $
                it "should be [4, 8]" $
                    (every4th [1,2,3,4,5,6,7,8,9,10]) `shouldBe` [4,8]

            context "every4th [4,5,6,4,3,2,3,4,5,7,8,2]" $
                it "should be [4,4,2]" $
                    (every4th [4,5,6,4,3,2,3,4,5,7,8,2]) `shouldBe` [4,4,2]
        
        describe "tupleDotProduct" $ do
            context "tupleDotProduct [1,2] [1,2]" $
                it "should be 5" $
                    (tupleDotProduct [1,2] [1,2]) `shouldBe` 5

            context "tupleDotProduct [1,2,3,4,6] [1,2,4,5,7]" $
                it "should be 79" $
                    (tupleDotProduct [1,2,3,4,6] [1,2,4,5,7]) `shouldBe` 79

        describe "appendToEach" $ do
            context "appendToEach '!!!' ['Hello', 'Goodbye']" $
                it "should be ['Hello!!!', 'Goodbye!!!']" $
                    (appendToEach "!!!" ["Hello", "Goodbye"]) `shouldBe` ["Hello!!!", "Goodbye!!!"]

            context "appendToEach 'why' ['hello', 'byebye', 'hihi','sup']" $
                it "should be ['hellowhy','byebyewhy','hihiwhy','supwhy']" $
                    (appendToEach "why" ["hello", "byebye", "hihi","sup"]) `shouldBe` ["hellowhy","byebyewhy","hihiwhy","supwhy"]

        describe "toSetList" $ do
            context "toSetList [5,1,2,3,3,4,5,5]" $
                it "should be [1,2,3,4,5]" $
                    (toSetList [5,1,2,3,3,4,5,5]) `shouldBe` [1,2,3,4,5]

            context "toSetList [5,4,2,12,3,5,6,7,8,6,5]" $
                it "should be [2,3,4,5,6,7,8,12]" $
                    (toSetList [5,4,2,12,3,5,6,7,8,6,5]) `shouldBe` [2,3,4,5,6,7,8,12]