module DailyTenSpec where

    import Test.Hspec

    import DailyTen

    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        describe "allLefts" $ do
            context "allLefts [Right 5, Left 4, Right 3]" $
                it "should be [4]" $
                    (allLefts [Right 5, Left 4, Right 3]) `shouldBe` [4]
            
            context "allLefts [Right 3, Right 8, Right 90, Left 42]" $
                it "should be [42]" $
                    (allLefts [Right 3, Right 8, Right 90, Left 42]) `shouldBe` [42]
                
            context "allLefts [Left 12, Left 80, Left 4, Right 8, Left 2]" $
                it "should be [Left 12, Left 80, Left 4, Left 2]" $
                    (allLefts [Left 12, Left 80, Left 4, Right 8, Left 2]) `shouldBe` [12, 80, 4, 2]
            
        describe "produceStringOrSum" $ do
            context "produceStringOrSum (Left 'hei') (Left 'no')" $
                it "should be Left 'hei'" $
                    (produceStringOrSum (Left "hei") (Left "no")) `shouldBe` Left "hei"
            
            context "produceStringOrSum (Left 'hei') (Right 8)" $
                it "should be Left 'hei'" $
                    (produceStringOrSum (Left "hei") (Right 8)) `shouldBe` Left "hei"

            context "produceStringOrSum (Right 8) (Right 8)" $
                it "should be Right 16" $
                    (produceStringOrSum (Right 8) (Right 8)) `shouldBe` Right 16

        describe "sumListOfEither" $ do
            context "sumListOfEither [Right 2]" $
                it "should be Right 2" $
                    (sumListOfEither [Right 2]) `shouldBe` Right 2
            
            context "sumListOfEither [Right 3, Right 2, Right 7]" $
                it "should be Right 12" $
                    (sumListOfEither [Right 3, Right 2, Right 7]) `shouldBe` Right 12

            context "sumListOfEither [Right 2, Right 3, Left 'email', Right 12]" $
                it "should be Left 'email'" $
                    (sumListOfEither [Right 2, Right 3, Left "email", Right 12]) `shouldBe` Left "email" 