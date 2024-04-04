module ExamplesTwoSpec where
    import Test.Hspec
    import ExamplesTwo

    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        describe "doubleList" $ do
            context "doubleList [1,2,3]" $
                it "should be [2,4,6]" $
                    (doubleList [1,2,3]) `shouldBe` [2,4,6]