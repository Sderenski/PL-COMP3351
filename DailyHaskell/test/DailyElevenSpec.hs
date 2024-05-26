module DailyElevenSpec where

    import Test.Hspec

    import DailyEleven

    import Data.Char (chr, isAlpha)
    
    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        describe "firstFunctorLaw" $ do
            it "holds for just ('c', 35)" $
                firstFunctorLaw (Just ('c', 35)) `shouldBe` True
            
            it "holds for [2, 3, 5, 7, 11]" $
                firstFunctorLaw [2,3,5,7,11] `shouldBe` True

        describe "secondFunctorLaw" $ do
            it "holds for isAlpha and fst with Just ('c', 35)" $
                secondFunctorLaw isAlpha fst (Just ('c', 35)) `shouldBe` True

            it "holds for chr and (+96) with [2,3,5,7,11]" $
                secondFunctorLaw chr (+96) [2,3,5,7,11] `shouldBe` True
                
        describe "Functor laws for Either String (Maybe Integer)" $ do
            it "firstFunctorLaw holds for Left \"error\"" $
                firstFunctorLaw (Left "error" :: Either String (Maybe Integer)) `shouldBe` True

            it "firstFunctorLaw holds for Right Nothing" $
                firstFunctorLaw (Right Nothing :: Either String (Maybe Integer)) `shouldBe` True

            it "firstFunctorLaw holds for Right (Just 42)" $
                firstFunctorLaw (Right (Just 42) :: Either String (Maybe Integer)) `shouldBe` True

            it "secondFunctorLaw holds for Left \"error\"" $
                secondFunctorLaw g f (Left "error" :: Either String (Maybe Integer)) `shouldBe` True

            it "secondFunctorLaw holds for Right Nothing" $
                secondFunctorLaw g f (Right Nothing :: Either String (Maybe Integer)) `shouldBe` True

            it "secondFunctorLaw holds for Right (Just 42)" $
                secondFunctorLaw g f (Right (Just 42) :: Either String (Maybe Integer)) `shouldBe` True