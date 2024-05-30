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
            
            context "removeChar 'e' ''" $
                it "should be ''" $
                    (removeChar 'e' "") `shouldBe` ""
        
        describe "removeWhitespace" $ do
            context "removeWhitespace 'Hello World!'" $
                it "should be 'HelloWorld!'" $
                    (removeWhitespace "Hello World!") `shouldBe` "HelloWorld!"

            context "removeWhitespace 'Greetings \n Everyone'" $
                it "should be 'GreetingsEveryone'" $
                    (removeWhitespace "Greetings \n Everyone") `shouldBe` "GreetingsEveryone"
            
            context "removeWhitespace ''" $
                it "should be ''" $
                    (removeWhitespace "") `shouldBe` ""

        describe "removePunctuation" $ do
            context "removePunctuation ''" $
                it "should be ''" $
                    (removePunctuation "") `shouldBe` ""

            context "removePunctuation '(Figure things out!!)'" $
                it "should be 'Figure things out'" $
                    (removePunctuation "(Figure things out!!)") `shouldBe` "Figure things out!!"

            context "removePunctuation '[Figure} things, out!!)'" $
                it "should be 'Figure things out'" $
                    (removePunctuation "[Figure} things, out!!)") `shouldBe` "Figure things out!!"

            context "removePunctuation '{Figure] things.)'" $
                it "should be 'Figure things out'" $
                    (removePunctuation "{Figure] things.)") `shouldBe` "Figure things"

        describe "charsToAscii" $ do
            context "charsToAscii []" $
                it "should be []" $
                    (charsToAscii []) `shouldBe` []

            context "charsToAscii ['a']" $
                it "should be [97]" $
                    (charsToAscii ['a']) `shouldBe` [97]
            
            context "charsToAscii ['h','e','l','p']" $
                it "should be [104, 101, 108, 112]" $
                    (charsToAscii ['h','e','l','p']) `shouldBe` [104, 101, 108, 112]
        
        describe "asciiToChars" $ do
            context "asciiToChar []" $
                it "should be []" $
                    (asciiToChars []) `shouldBe` []

            context "asciiToChar [104, 101, 108, 112]" $
                it "should be ['h','e','l','p']" $
                    (asciiToChars [104, 101, 108, 112]) `shouldBe` ['h','e','l','p']

        describe "shiftInts" $ do 
            context "shiftInts 1 []" $
                it "should be []" $
                    (shiftInts 1 []) `shouldBe` []

            context "shiftInts 1 [2,4,6,127]" $
                it "should be [3,5,7,0]" $
                    (shiftInts 1 [2,4,6,127]) `shouldBe` [3,5,7,0]

            context "shiftInts (-1) [2,4,6,127]" $
                it "should be [1,3,5,126]" $
                    (shiftInts (-1) [2,4,6,127]) `shouldBe` [1,3,5,126]
            
            context "shiftInts (-2) [1,3,6,127]" $
                it "should be [127,1,4,125]" $
                    (shiftInts (-2) [1,3,6,127]) `shouldBe` [127,1,4,125]
            
            context "shiftInts 3 [2,3,100,63]" $
                it "should be [5,6,103,66]" $
                    (shiftInts 3 [2,3,100,63]) `shouldBe` [5,6,103,66]

        describe "shiftMessage" $ do
            context "shiftMessage 1 []" $
                it "should be []" $
                    (shiftMessage 1 []) `shouldBe` []

            context "shiftMessage 1 'hello'" $
                it "should be 'iffp'" $
                    (shiftMessage 1 "hello") `shouldBe` "ifmmp"

            context "shiftMessage (-1) 'ifmmp'" $
                it "should be 'hello'" $
                    (shiftMessage (-1) "ifmmp") `shouldBe` "hello"

        
