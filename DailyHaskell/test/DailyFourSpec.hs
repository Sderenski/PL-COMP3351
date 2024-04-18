module DailyFourSpec where

    import Test.Hspec

    import DailyFour

    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        describe "zip3Lists" $ do
            context "zip3Lists [1,2,3] ['a','b','c'] [4,5,6]" $
                it "should be [(1,'a',4), (2,'b',5), (3,'c',6)]" $
                    (zip3Lists [1,2,3] ['a','b','c'] [4,5,6]) `shouldBe` [(1,'a',4), (2,'b',5), (3,'c',6)]

            context "zip3Lists [5,4,3,2,3] ['a','b','c','d','e'] [4,5,6,1,2]" $
                it "should be [(1,'a',4), (2,'b',5), (3,'c',6), (2,'d',1), (3,'e',2)]" $
                    (zip3Lists [1,2,3,2,3] ['a','b','c','d','e'] [4,5,6,1,2]) `shouldBe` [(1,'a',4), (2,'b',5), (3,'c',6), (2,'d',1), (3,'e',2)]

            context "zip3Lists [1] ['a'] [4]" $
                it "should be [(1,'a',4)]" $
                    (zip3Lists [1] ['a'] [4]) `shouldBe` [(1,'a',4)]

        describe "unzipTriples"$ do
            context "unzipTriples [(1,2,3), (4,5,6), (7,8,9)]" $
                it "should be ([1,4,7], [2,5,8], [3,6,9])" $
                    (unzipTriples [(1,2,3), (4,5,6), (7,8,9)]) `shouldBe` ([1,4,7], [2,5,8], [3,6,9])

            context "unzipTriples [(1,2,3)]" $
                it "should be ([1], [2], [3])" $
                    (unzipTriples [(1,2,3)]) `shouldBe` ([1], [2], [3])
            
            context "unzipTriples [('q','s','w'), ('e','t','r'), ('b','n','m')]" $
                it "should be (['q','e','b'], ['s','t','n'], ['w','r','m'])" $
                    (unzipTriples [('q','s','w'), ('e','t','r'), ('b','n','m')]) `shouldBe` (['q','e','b'], ['s','t','n'], ['w','r','m'])
        
        describe "mergeSorted3" $ do
            context "mergeSorted3 [2,3,5] [1,8] [-1,0,4,10]" $
                it "should be [-1,0,1,2,3,4,5,8,10]" $
                    (mergeSorted3 [2,3,5] [1,8] [-1,0,4,10]) `shouldBe` [-1,0,1,2,3,4,5,8,10]

            context "mergeSorted3 [2,3,5,9] [] []" $
                it "should be [2,3,5,9]" $
                    (mergeSorted3 [2,3,5,9] [] []) `shouldBe` [2,3,5,9]
            
            context "mergeSorted3 [2,3] [] [-1,0,2,3,4,5,20]" $
                it "should be [-1,0,2,2,3,3,4,5,10]" $
                    (mergeSorted3 [2,3] [] [-1,0,2,3,4,5,20]) `shouldBe` [-1,0,2,2,3,3,4,5,20]