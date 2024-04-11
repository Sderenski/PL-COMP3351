module WeeklyHaskellOneSpec where
    
    import Test.Hspec

    import WeeklyHaskellOne
    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        describe "removeChar" $ do

            context "removeChar 'w' 'weekly'" $
                it "should be 'eekly'" $
                    (removeChar 'w' "weekly") `shouldBe` "eekly"
            
            context "removeChar 'e' 'weekly'" $
                it "should be 'wkly'" $
                    (removeChar 'e' "weekly") `shouldBe` "wkly"

            context "removeChar 'i' 'difficulties'" $
                it "should be 'dffcultes'" $
                    (removeChar 'i' "difficulties") `shouldBe` "dffcultes"
        
        describe "charsToAscii" $ do

            context "charsToAscii ['a']" $
                it "should be [97]" $
                    (charsToAscii ['a']) `shouldBe` [97]

        
