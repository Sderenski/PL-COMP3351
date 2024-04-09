module DailyTwoSpec where
    import Test.Hspec
    import DailyTwo

    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        -- describe "every4th" $ do
        --     context "every4th [1,2,3,4,5,6,7,8,9,10]" $
        --         it "should be [4, 8]" $
        --             (every4th [1,2,3,4,5,6,7,8,9,10]) `shouldBe` [4,8]
        
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