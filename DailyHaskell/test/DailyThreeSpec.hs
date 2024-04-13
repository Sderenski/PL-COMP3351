module DailyThreeSpec where
    
    import Test.Hspec
    import DailyThree
    
    main :: IO ()
    main = hspec spec

    spec :: Spec 
    spec = do 
        describe "removeAllExcept" $ do
            context "removeAllExcept 1 [2,3,4,1]" $
                it "should be [1]" $
                    (removeAllExpect 1 [2,3,4,1]) `shouldBe` [1]

            context "removeAllExcept 'a' ['b','a','c','a']" $
                it "should be ['a','a']" $
                    (removeAllExpect 'a' ['b','a','c','a']) `shouldBe` ['a','a']

        describe "countOccurences" $ do
            context "countOccurences 1 [2,4,5,2]" $
                it "should be 0" $
                    (countOccurences 1 [2,4,5,2]) `shouldBe` 0
            
            context "countOccurences 'a' ['a','b','a','c']" $
                it "should be 2" $
                    (countOccurences 'a' ['a','b','a','c']) `shouldBe` 2
        
        describe "substitute" $ do 
            context "substitute 3 4 [1,2,3,4]" $
                it "should be [1,2,4,4]" $
                    (substitute 3 4 [1,2,3,4]) `shouldBe` [1,2,4,4]

            context "substitute 'a' 'b' ['a','c','a','b']" $
                it "should be ['b','c','b','b']" $
                    (substitute 'a' 'b' ['a','c','a','b']) `shouldBe` ['b','c','b','b']
