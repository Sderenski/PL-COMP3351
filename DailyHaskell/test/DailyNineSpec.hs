module DailyNineSpec where

    import Test.Hspec

    import DailyNine

    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        describe "onlyNothing" $ do
            context "when all elements produce Nothing" $ do
                it "returns True" $
                    onlyNothing (\_ -> Nothing) [1, 2, 3] `shouldBe` True
            
            context "when some elements produce Just and others produce Nothing" $ do
                it "returns False" $
                    onlyNothing (\x -> if x `mod` 2 == 0 then Just "even" else Nothing) [1, 2, 3, 4] `shouldBe` False
            
            context "when all elements produce Just" $ do
                it "returns False" $
                    onlyNothing (\_ -> Just "something") [1, 2, 3] `shouldBe` False

        describe "firstAnswer" $ do
            context "when some elements produce Just and others produce Nothing" $ do
                it "returns the first Just value encountered" $
                    firstAnswer (\x -> if x `mod` 2 == 0 then Just (x * 2) else Nothing) [1, 2, 3, 4] `shouldBe` Just 4
            
            context "when all elements produce Nothing" $ do
                it "returns Nothing" $
                    firstAnswer (\_ -> Nothing) [1, 2, 3] `shouldBe` (Nothing :: Maybe Int) -- specifying type explicitly
            
            context "when all elements produce Just" $ do
                it "returns the first Just value encountered" $
                    firstAnswer (\x -> Just (x * 2)) [1, 2, 3] `shouldBe` Just 2

        describe "allAnswers" $ do
            context "when some elements produce Nothing" $ do
                it "returns Nothing" $
                    allAnswers (\x -> if x `mod` 2 == 0 then Just [x * 2] else Nothing) [1, 2, 3, 4] `shouldBe` Nothing
            
            context "when all elements produce Just" $ do
                it "returns the list of Just values" $
                    allAnswers (\x -> Just [x * 2]) [1, 2, 3] `shouldBe` Just [2, 4, 6]
            
            context "when some elements produce empty lists" $ do
                it "returns the concatenated list of Just values" $
                    allAnswers (\x -> if x `mod` 2 == 0 then Just [] else Just [x * 2]) [1, 2, 3, 4] `shouldBe` Just [2, 6]