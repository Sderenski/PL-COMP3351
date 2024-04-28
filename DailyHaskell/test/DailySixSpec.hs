module DailySixSpec where

    import Test.Hspec 

    import DailySix

    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        describe "shorterThan" $ do
            context "shorterThan 3 ['go','Run','taught']" $
                it "should be ['go','Run']" $
                    (shorterThan 3 ["go", "Run","taught"]) `shouldBe` ["go","Run"]
            
            context "shorterThan 2 ['go','Run','taught']" $
                it "should be ['go']" $
                    (shorterThan 2 ["go", "Run","taught"]) `shouldBe` ["go"]

            context "shorterThan 5 ['go','Run','taught', 'beautiful']" $
                it "should be ['go','Run']" $
                    (shorterThan 5 ["go", "Run","taught", "beautiful"]) `shouldBe` ["go","Run"]

        describe "removeMultiple" $ do
            context "removeMultiple 5 [3,5,9,10,15]" $
                it "should be [3,9]" $
                    (removeMultiple 5 [3,5,9,10,15]) `shouldBe` [3,9]

            context "removeMultiple 3 [3,5,9,10,15]" $ 
                it "should be [5,10]" $
                    (removeMultiple 3 [3,5,9,10,15]) `shouldBe` [5,10]

            context "removeMultiple 8 [2,3,5,8,16,23,24,31]" $
                it "should be [2,3,5,23,31]" $
                    (removeMultiple 8 [2,3,5,8,16,23,24,31]) `shouldBe` [2,3,5,23,31]
        
        describe "onlyJust" $ do
            context "onlyJust [Nothing, Just 5, Nothing, Just 10]" $
                it "should be [Just 5, Just 10]" $
                    (onlyJust [Nothing, Just 5, Nothing, Just 10]) `shouldBe` [Just 5, Just 10]
            
            context "onlyJust [Just 9, Just 2, Nothing, Nothing, Just 3]" $
                it "should be [Just 9, Just 2, Just 3]" $
                    (onlyJust [Just 9, Just 2, Nothing, Nothing, Just 3]) `shouldBe` [Just 9, Just 2, Just 3]

            context "onlyJust [Just 2]" $
                it "should be [Just 2]" $
                    (onlyJust [Just 2]) `shouldBe` [Just 2]