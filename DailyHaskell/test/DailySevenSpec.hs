module DailySevenSpec where

    import Test.Hspec

    import DailySeven

    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        describe "findLongest" $ do
            context "findLongest ['Hi','go','far']" $
                it "should be 'far'" $
                    (findLongest ["Hi", "go", "far"]) `shouldBe` "far"
            
            context "findLongest ['Highest','go','far']" $
                it "should be 'Highest'" $
                    (findLongest ["Highest", "go", "far"]) `shouldBe` "Highest"

            context "findLongest ['Hi','going','far']" $
                it "should be 'going'" $
                    (findLongest ["Hi", "going", "far"]) `shouldBe` "going"

        describe "anyLarger" $ do
            context "anyLarger 5 [1,2,3,2]" $
                it "should be False" $
                    (anyLarger 5 [1,2,3,2]) `shouldBe` False

            context "anyLarger 5 [1,2,4,7,3,10]" $
                it "should be True" $
                    (anyLarger 5 [1,2,4,7,3,10])  `shouldBe` True
            
            context "anyLarger 2 [1]" $
                it "should be False" $
                    (anyLarger 2 [1]) `shouldBe` False

        describe "allNames" $ do
            context "allNames [('Stephen','Derenski'), ('Zack','Baylor')]" $
                it "should be 'Stephen Derenski, Zack Baylor'" $
                    (allNames [("Stephen","Derenski"), ("Zack","Baylor")]) `shouldBe` "Stephen Derenski, Zack Baylor"

            context "allNames [('Sabina','Triska')]" $
                it "should be 'Sabrina Triska'" $
                    (allNames [("Sabrina","Triska")]) `shouldBe` "Sabrina Triska"

            context "allNames [('Kyle','W'), ('Zack','Baylor')]" $
                it "should be 'Kyle W, Zack Baylor'" $
                    (allNames [("Kyle","W"), ("Zack","Baylor")]) `shouldBe` "Kyle W, Zack Baylor"
